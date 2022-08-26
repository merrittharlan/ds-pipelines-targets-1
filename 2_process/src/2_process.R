library(dplyr)
library(stringr)
library(whisker)

#' Save the model summary, filter and add plotting attributes to model comparison
#' 
#' @param modeldata data downloaded in 1_fetch (model summary csv for this example)
#' @param modelfile file name and directory of model diagnostics
#' @param pbColor color of Process-Based model
#' @param dlColor color of Deep Learning model
#' @param pgdlColor color of Process-Guided Deep Learning model
#' @param pbPch plot character of Process-Based model
#' @param dlPch plot character of Deep Learning model
#' @param pgdlPch plot character of Process-Guided Deep Learning model
#' @return filtered model data from science base with visualization parameters

process_data <- function(in_filepath = "1_fetch/out/model_RMSEs.csv", 
                         pbColor = '#1b9e77',
                         dlColor = '#d95f02',
                         pgdlColor = '#7570b3',
                         pbPch = 21,
                         dlPch = 22,
                         pgdlPch = 23
                         ){
  project_output_dir <- '2_process/out'
  if (!dir.exists(project_output_dir)){
    dir.create(project_output_dir)
  }
  
  modeldata = readr::read_csv(in_filepath, col_types = 'iccd')
 
  eval_data = modeldata %>%
    filter(str_detect(exper_id, 'similar_[0-9]+')) %>%
    mutate(
      col = case_when(
        model_type == 'pb' ~ pbColor,
        model_type == 'dl' ~ dlColor,
        model_type == 'pgdl' ~ pgdlColor
      ),
      pch = case_when(
        model_type == 'pb' ~ pbPch,
        model_type == 'dl' ~ dlPch,
        model_type == 'pgdl' ~ pgdlPch
      ),
      n_prof = as.numeric(str_extract(exper_id, '[0-9]+'))
    )
  readr::write_csv(eval_data, file = "2_process/out/model_summary_results.csv")
  return(eval_data)
}


#' Save the model diagnostics
#' 
#' @param data data downloaded in 1_fetch (model summary csv for this example)
#' @param out_filepath file name and directory of model diagnostics
#' @return save the model diagnostics

generate_model_diagnostics <- function(data = eval_data,
                                   out_filepath = "2_process/out/model_diagnostic_text.txt"){

  mean_data = data %>% group_by(model_type, exper_id) %>%
    summarize(mean_rmse = round(mean(rmse), 2)) %>%
    mutate(Name = paste0(paste0(model_type, exper_id, "mean")))
  render_data = list(mean_data$mean_rmse)[[1]]
  names(render_data) = gsub("similar", "", mean_data$Name) 
  
  template_1 = 'resulted in mean RMSEs (means calculated as average of RMSEs from the five dataset iterations) of {{pgdl_980mean}}, {{dl_980mean}}, and {{pb_980mean}}°C for the PGDL, DL, and PB models, respectively.
  The relative performance of DL vs PB depended on the amount of training data. The accuracy of Lake Mendota temperature predictions from the DL was better than PB when trained on 500 profiles 
  ({{dl_500mean}} and {{pb_500mean}}°C, respectively) or more, but worse than PB when training was reduced to 100 profiles ({{dl_100mean}} and {{pb_100mean}}°C respectively) or fewer.
  The PGDL prediction accuracy was more robust compared to PB when only two profiles were provided for training ({{pgdl_2mean}} and {{pb_2mean}}°C, respectively). '
  
  whisker.render(template_1 %>% str_remove_all('\n') %>% str_replace_all('  ', ' '), render_data ) %>% cat(file = out_filepath)
  return(out_filepath)
}

