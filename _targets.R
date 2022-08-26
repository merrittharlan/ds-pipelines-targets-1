library(targets)
source("1_fetch/src/1_fetch.R")
source("2_process/src/2_process.R")
source("3_visualize/src/3_visualize.R")

tar_option_set(packages = c("tidyverse", "sbtools", "whisker"))


list(
  # Get the data from ScienceBase
  tar_target(
    model_RMSEs_csv,
    download_data(out_filepath = "1_fetch/out/model_RMSEs.csv"),
    format = "file"
  ),
  # Prepare the data for plotting
  tar_target(
    eval_data,
    process_data(in_filepath = model_RMSEs_csv),
  ),
  # Create a plot
  tar_target(
    figure_1_png,
    make_plot(out_filepath = "3_visualize/out/figure_1.png", data = eval_data),
    format = "file"
  ),
  # Save the model diagnostics
  tar_target(
    model_diagnostic_text_txt,
    generate_model_diagnostics(out_filepath = "2_process/out/model_diagnostic_text.txt", data = eval_data),
    format = "file"
  )
)
