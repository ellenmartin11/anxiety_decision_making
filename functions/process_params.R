## parameter analyses functions

plot_happiness_model_parameters <- function(data) {
  data %>%
  group_by(userKey) %>%
    summarise(
      mean_RawHappy = mean(Happy_filled, na.rm = TRUE),
      sd_RawHappy = sd(Happy_filled, na.rm = TRUE),
      rsd_RawHappy = relSD_tc(HappyRating, 0, 100),
      GAD_score = first(gad7_total),
      PHQ_score = first(phq8_total),
      gamble_decisions = sum(Choice == 2, na.rm = TRUE),
      total_decisions = n(),
      percent_gamble = (gamble_decisions / total_decisions) * 100,
      const = first(const1_multi),
      tau = first(tau_multi),
      rpe = first(rpe_chosen_multi),
      ev = first(ev_chosen_multi)
    ) %>%
    na.omit()
  
  summary_plot_data <- data %>%
    dplyr::select(ev_chosen_multi, rpe_chosen_multi) %>%
    pivot_longer(cols = c(ev_chosen_multi, rpe_chosen_multi), 
                 names_to = "parameter", 
                 values_to = "value") %>%
    group_by(parameter) %>%
    summarise(
      mean_value = mean(value, na.rm = TRUE),
      se_value = sd(value, na.rm = TRUE) / sqrt(n())
    ) %>%
    na.omit()
  
  # Generate the plot
  ggplot(summary_plot_data, aes(x = parameter, y = mean_value)) +
    geom_bar(stat = "identity", position = position_dodge(), width = 0.7, fill = "lightblue", color = "black") +
    geom_errorbar(aes(ymin = mean_value - se_value, ymax = mean_value + se_value),
                  position = position_dodge(0.7), width = 0.2) +
    geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 0.5) +
    labs(
      title = "Happiness Model Parameters",
      x = "Parameter",
      y = "Parameter Estimate"
    ) +
    theme_minimal() +
   poster_theme  + 
    scale_x_discrete(labels = c(
      "ev_chosen_multi" = "EV", 
      "rpe_chosen_multi" = "RPE"))
}

plot_happiness_model_parameters_z <- function(data) {
  data %>%
    group_by(userKey) %>%
    summarise(
      mean_RawHappy = mean(Happy_filled, na.rm = TRUE),
      sd_RawHappy = sd(Happy_filled, na.rm = TRUE),
      rsd_RawHappy = relSD_tc(HappyRating, 0, 100),
      GAD_score = first(gad7_total),
      PHQ_score = first(phq8_total),
      gamble_decisions = sum(Choice == 2, na.rm = TRUE),
      total_decisions = n(),
      percent_gamble = (gamble_decisions / total_decisions) * 100,
      const_z = first(const1_multi_z),
      tau_z = first(tau_multi_z),
      rpe_z = first(rpe_chosen_multi_z),
      ev_z = first(ev_chosen_multi_z)
    ) %>%
    na.omit()
  
  summary_plot_data <- data %>%
    dplyr::select(ev_chosen_multi_z, rpe_chosen_multi_z) %>%
    pivot_longer(cols = c(ev_chosen_multi_z, rpe_chosen_multi_z), 
                 names_to = "parameter", 
                 values_to = "value") %>%
    group_by(parameter) %>%
    summarise(
      mean_value = mean(value, na.rm = TRUE),
      se_value = sd(value, na.rm = TRUE) / sqrt(n())
    ) %>%
    na.omit()
  
  # Generate the plot
  ggplot(summary_plot_data, aes(x = parameter, y = mean_value)) +
    geom_bar(stat = "identity", position = position_dodge(), width = 0.7, fill = "lightblue", color = "black") +
    geom_errorbar(aes(ymin = mean_value - se_value, ymax = mean_value + se_value),
                  position = position_dodge(0.7), width = 0.2) +
    geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 0.5) +
    labs(
      title = "Happiness Model Parameters",
      x = "Parameter",
      y = "Parameter Estimate"
    ) +
    theme_minimal() +
    poster_theme  + 
    scale_x_discrete(labels = c(
      "ev_chosen_multi_z" = "EV", 
      "rpe_chosen_multi_z" = "RPE"))
}

plot_happiness_model_parameters_all <- function(data) {
  data %>%
    group_by(userKey) %>%
    summarise(
      mean_RawHappy = mean(RawHappy, na.rm = TRUE),
      sd_RawHappy = sd(RawHappy, na.rm = TRUE),
      rsd_RawHappy = relSD_tc(HappyRating, 0, 100),
      GAD_score = first(gad7_total),
      PHQ_score = first(phq8_total),
      gamble_decisions = sum(Choice == 2, na.rm = TRUE),
      total_decisions = n(),
      percent_gamble = (gamble_decisions / total_decisions) * 100,
      const = first(const1_multi),
      tau = first(tau_multi),
      rpe = first(rpe_chosen_multi),
      ev = first(ev_chosen_multi)
    ) %>%
    na.omit()
  
  summary_plot_data <- data %>%
    dplyr::select(ev_chosen_multi, rpe_chosen_multi, tau_multi, const1_multi) %>%
    pivot_longer(cols = c(ev_chosen_multi, rpe_chosen_multi, tau_multi, const1_multi), 
                 names_to = "parameter", 
                 values_to = "value") %>%
    group_by(parameter) %>%
    summarise(
      mean_value = mean(value, na.rm = TRUE),
      se_value = sd(value, na.rm = TRUE) / sqrt(n())
    ) %>%
    na.omit()
  
  # Generate the plot
  ggplot(summary_plot_data, aes(x = parameter, y = mean_value)) +
    geom_bar(stat = "identity", position = position_dodge(), width = 0.7, fill = "lightblue", color = "black") +
    geom_errorbar(aes(ymin = mean_value - se_value, ymax = mean_value + se_value),
                  position = position_dodge(0.7), width = 0.2) +
    geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 0.5) +
    labs(
      title = "Happiness Model Parameters",
      x = "Parameter",
      y = "Parameter Estimate"
    ) +
    theme_minimal() +
    poster_theme  + 
    scale_x_discrete(labels = c(
      "ev_chosen_multi" = "EV", 
      "rpe_chosen_multi" = "RPE",
      "tau_multi"= "Tau",
      "const1_multi" = "Baseline Happiness"))
}

# plotting parameters split by group
plot_summary_stats <- function(data, group_var) {
  # Ensure the group_var is a column in the data
  if (!group_var %in% colnames(data)) {
    stop("The specified group_var is not a column in the data.")
  }
  
  # Prepare the data
  summary_happyData_long <- data %>%
    dplyr::select(userKey, all_of(group_var), abs_rpe, abs_ev) %>%
    pivot_longer(cols = c(abs_rpe, abs_ev), 
                 names_to = "variable", values_to = "value")
  
  summary_stats <- summary_happyData_long %>%
    group_by(.data[[group_var]], variable) %>%
    summarise(
      mean_value = mean(value, na.rm = TRUE),
      se_value = sd(value, na.rm = TRUE) / sqrt(n())
    ) %>% na.omit()
  
  # Generate the plot
  ggplot(summary_stats, aes(x = variable, y = mean_value, fill = .data[[group_var]])) +
    geom_bar(stat = "identity", position = position_dodge(), width = 0.7, color = "black") +
    geom_errorbar(aes(ymin = mean_value - se_value, ymax = mean_value + se_value),
                  position = position_dodge(0.7), width = 0.2) +
    labs(
      x = "Parameter",
      y = "Parameter Estimate",
      fill = group_var
    ) +
    theme_minimal() +
    poster_theme + 
    scale_fill_manual(values = c("GAD 6+" = "#00BD9D", "GAD < 6" = "#8BD7D2", "PHQ 7+" = "red", "PHQ < 7" = "pink","Female"="#7D80DA", "Male"="#CEBACF")) +
    scale_x_discrete(labels = c("abs_ev" = "abs(EV)", "abs_rpe" = "abs(RPE)"))
}

plot_summary_stats_z <- function(data, group_var) {
  # Ensure the group_var is a column in the data
  if (!group_var %in% colnames(data)) {
    stop("The specified group_var is not a column in the data.")
  }
  
  # Prepare the data
  summary_happyData_long <- data %>%
    dplyr::select(userKey, all_of(group_var), abs_rpe_z, abs_ev_z) %>%
    pivot_longer(cols = c(abs_rpe_z, abs_ev_z), 
                 names_to = "variable", values_to = "value")
  
  summary_stats <- summary_happyData_long %>%
    group_by(.data[[group_var]], variable) %>%
    summarise(
      mean_value = mean(value, na.rm = TRUE),
      se_value = sd(value, na.rm = TRUE) / sqrt(n())
    ) %>% na.omit()
  
  # Generate the plot
  ggplot(summary_stats, aes(x = variable, y = mean_value, fill = .data[[group_var]])) +
    geom_bar(stat = "identity", position = position_dodge(), width = 0.7, color = "black") +
    geom_errorbar(aes(ymin = mean_value - se_value, ymax = mean_value + se_value),
                  position = position_dodge(0.7), width = 0.2) +
    labs(
      x = "Parameter",
      y = "Parameter Estimate",
      fill = group_var
    ) +
    theme_minimal() +
    poster_theme + 
    scale_fill_manual(values = c("GAD 6+" = "#00BD9D", "GAD < 6" = "#8BD7D2", "PHQ 7+" = "red", "PHQ < 7" = "pink","Female"="#7D80DA", "Male"="#CEBACF")) +
    scale_x_discrete(labels = c("abs_ev_z" = "abs(EV)", "abs_rpe_z" = "abs(RPE)"))
}

plot_summary_stats_z_directional <- function(data, group_var) {
  # Ensure the group_var is a column in the data
  if (!group_var %in% colnames(data)) {
    stop("The specified group_var is not a column in the data.")
  }
  
  # Prepare the data
  summary_happyData_long <- data %>%
    dplyr::select(userKey, all_of(group_var), rpe_chosen_multi_z, ev_chosen_multi_z) %>%
    pivot_longer(cols = c(rpe_chosen_multi_z, ev_chosen_multi_z), 
                 names_to = "variable", values_to = "value")
  
  summary_stats <- summary_happyData_long %>%
    group_by(.data[[group_var]], variable) %>%
    summarise(
      mean_value = mean(value, na.rm = TRUE),
      se_value = sd(value, na.rm = TRUE) / sqrt(n())
    ) %>% na.omit()
  
  # Generate the plot
  ggplot(summary_stats, aes(x = variable, y = mean_value, fill = .data[[group_var]])) +
    geom_bar(stat = "identity", position = position_dodge(), width = 0.7, color = "black") +
    geom_errorbar(aes(ymin = mean_value - se_value, ymax = mean_value + se_value),
                  position = position_dodge(0.7), width = 0.2) +
    labs(
      x = "Parameter",
      y = "Parameter Estimate",
      fill = group_var
    ) +
    theme_minimal() +
    poster_theme + 
    scale_fill_manual(values = c("GAD 6+" = "#00BD9D", "GAD < 6" = "#8BD7D2", "PHQ 7+" = "red", "PHQ < 7" = "pink","Female"="#7D80DA", "Male"="#CEBACF")) +
    scale_x_discrete(labels = c("ev_chosen_multi_z" = "EV", "rpe_chosen_multi_z" = "RPE"))
}
