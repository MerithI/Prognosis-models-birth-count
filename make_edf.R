######################################################################
#'*Make Emperical Probability Distribution around due date*
######################################################################
# For each unique dif (birth date - due date), the frequency is divided by the nr of total births
# This can be calculated for all groups within a category at once. 
# Then for each group, this frequency/total ratio is calculated within the respective group. 

# Suitable for distribution for different subcategories (plotted in 1 visual)
make_edf_var <- function(df, dif_variable, group, linesize = 0.8, width = 4.5, col = FALSE){
  
  
  # Preparation
  group_name <- deparse(substitute(group))

  unique_groups <- unique(df[[deparse(substitute(group))]])
  
  sequence <- seq(-126, 35)
  
  df_tmp <- tibble(
    groups = rep(unique_groups, each = length(sequence)),
    dif   = rep(sequence, times = length(unique_groups))
  ) %>%
    rename_with(~group_name, groups)

  
  #Make empirical distribution + probability
  df_edf <- df %>%
    group_by({{group}}, {{dif_variable}}) %>%
    count() %>%
    group_by({{group}}) %>%
    mutate(p = n/sum(n),
           count = n) %>%
    ungroup() 
  
  print(df_edf)
  
  df_tmp <- df_tmp %>%
    left_join(df_edf, by = c(group_name, "dif" = "dif")) %>%
    mutate(across(where(is.numeric), ~replace_na(.,0))) 
  
  if(is.factor(df_tmp[[1]])){
    df_tmp <- df_tmp %>%
     mutate(across(where(is.factor), ~forcats::fct_explicit_na(.,na_level ="Unknown")))
    }
  
  if(is.character(df_tmp[[1]])){
    df_tmp <- df_tmp %>%
  mutate(across(where(is.character), ~replace_na(.,"Unknown")))
    }
  
  
  #Plots
  p_tmp1 <- df_tmp %>%
    ggplot(aes(x={{dif_variable}}, y = p, color = {{group}}, group = {{group}})) +
    geom_line(size = linesize) +
    ggtitle(paste("Emperical distribution")) +
    xlim(-56, 35) +  # kan weg
    # guides(colour = guide_legend(override.aes = list(size = 10))) +
    if(col){scale_color_brewer(palette = "Paired")}
    # scale_fill_paletteer_d("ggsci::hallmarks_light_cosmic", 10)
  
  print(p_tmp1)
  
  ggsave(paste0(setting_dir, "/", group_name, "edf1.jpeg"), plot = p_tmp1, width = width, height = 3, dpi = setting_dpi)
  
  return(df_tmp)

}


# Suitable for overall distribution of whole dataset
make_edf <- function(df, dif_variable, group)
{
  df_tmp <- df %>%
    group_by({{group}}, {{dif_variable}}) %>%
    count() %>%
    
    group_by({{group}}) %>%
    mutate(p = n/sum(n),
           count = n) %>%
    ungroup()
  
  
  p_tmp2 <- df_tmp %>%
    ggplot(aes(x={{dif_variable}}, y = p)) +
    geom_line() +  +
    ggtitle(paste("Emperical distribution")) 
  

  print(p_tmp2)

  ggsave(paste0(setting_dir, "/edf2.jpeg"), plot = p_tmp2, width = 6, height = 3, dpi = setting_dpi)
  return(df_tmp)
}





