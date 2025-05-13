#' bn_finalize
#' @description bn_finalize
#' @export
bn_finalize <- function(
    viz_prep_to_finalize,
    df,
    dv,
    subgroups,
    dictionary = NULL,
    traditional_driver_engine = c("linear", "logistic"),
    community_score_by = c("sum", "mean"),
    standardize_traditional_drivers = FALSE,
    vs_layout = "layout_with_fr"
){

  traditional_driver_engine <- match.arg(traditional_driver_engine)

  black_list = viz_prep_to_finalize[["nodes"]] %>%
    select(id) %>%
    unlist() %>%
    setNames(NULL) %>%
    list(., .) %>%
    map_dfr(make_arcs)


  bn <- bn_tan(
    df = df,
    dv = dv,
    ivs = viz_prep_to_finalize[["nodes"]][["id"]],
    white_list = viz_prep_to_finalize[["edges"]] %>% select(from, to),
    black_list = black_list,
    cross_battery_first = FALSE,
    suppress_bn_warning = TRUE
  )



  viz_prep_final <- bn_to_netviz_prep(
    bn = bn,
    dictionary = dictionary
  )



  viz_prep_final[["nodes"]] <- viz_prep_final[["nodes"]] %>%
    left_join(
      viz_prep_to_finalize[["nodes"]] %>%
        select(id, group, community_name),
      by = join_by(id)
    ) %>%
    rename(group = group.y) %>%
    select(-c(group.x, color)) %>%
    mutate(group = group %>% as.numeric()) %>%
    left_join(
      viz_prep_to_finalize[["nodes"]][["group"]] %>% max() %>% bn_community_color(),
      by = join_by("group")
    )


  results_raw_list <- subgroups %>% map(
    ~bn_tan(
      df = df %>%
        filter_at(
          .x, all_vars(equals(., 1))
        ),
      dv = dv,
      ivs = viz_prep_to_finalize[["nodes"]][["id"]],
      white_list = viz_prep_to_finalize[["edges"]] %>% select(from, to),
      black_list = black_list,
      cross_battery_first = FALSE,
      compare_to_niave = TRUE,
      suppress_bn_warning = TRUE
    )
  ) %>%
    setNames(subgroups) %>%
    map(pluck, "arcs")


  impact_attributes <- results_raw_list %>%
    map(pluck, "dv") %>%
    imap(
      ~.x %>%
        mutate(
          Impact = mi %>% map(~(.x/mean(abs(mi)))*100) %>% as.numeric()
        )
    ) %>%
    map(select, to, Impact, mi, pval) %>%
    imap(~setNames(.x, c("Variable", glue("{.y} {c('', 'MI', 'P Value')}")))) %>%
    plyr::join_all(by = "Variable") %>%
    left_join(
      dictionary %>% select(var, label),
      by = join_by(Variable == var)
    ) %>%
    relocate(label, .after = Variable) %>%
    left_join(
      viz_prep_final[["nodes"]] %>% select(id, community_name),
      by = join_by(Variable == id)
    ) %>%
    relocate(community_name, .after = Variable) %>%
    as_tibble() %>%
    setNames(., names(.) %>% trimws()) %>%
    arrange(-Total)


  impact_community_by_sum <- impact_attributes %>%
    select_if(stringr::str_detect(names(.), "MI|community_name")) %>%
    group_by(community_name) %>%
    summarise_if(is.numeric, sum) %>%
    mutate_if(is.numeric, function(y) y %>% map(~(.x/mean(abs(y)))*100) %>% as.numeric()) %>%
    setNames(., names(.) %>% gsub(" MI", "", .))


  impact_community_by_mean <- impact_attributes %>%
    select_if(stringr::str_detect(names(.), "MI|community_name")) %>%
    group_by(community_name) %>%
    summarise_if(is.numeric, mean) %>%
    mutate_if(is.numeric, function(y) y %>% map(~(.x/mean(abs(y)))*100) %>% as.numeric())



  attribute_community_contribution <- results_raw_list %>%
    map(pluck, "ivs") %>%
    map(
      function(x)rbind(
        x %>% select(from, mi) %>% setNames(c("Variable", "mi")),
        x %>% select(to, mi) %>% setNames(c("Variable", "mi"))
      ) %>%
        left_join(
          impact_attributes %>% select(Variable, community_name),
          by = join_by(Variable == Variable)
        ) %>%
        group_by(community_name, Variable) %>%
        summarise_at("mi", ~sum(.)/2) %>%
        group_by(community_name) %>%
        mutate(
          contribution = mi / sum(mi)
        ) %>%
        ungroup() %>%
        select(community_name, Variable, contribution)
    ) %>%
    plyr::join_all(by = c("Variable", "community_name")) %>%
    setNames(c("Variable", "Community Name", subgroups))


  comunity_edges <- results_raw_list[["Total"]][["ivs"]] %>%
    left_join(
      impact_attributes %>% select(Variable, community_name),
      by = join_by(from == Variable)
    ) %>%
    left_join(
      impact_attributes %>% select(Variable, community_name),
      by = join_by(to == Variable)
    ) %>%
    filter(community_name.x != community_name.y) %>%
    select(community_name.x, community_name.y, mi) %>%
    rename(
      from = community_name.x,
      to = community_name.y,
      value = mi
    )


  if(standardize_traditional_drivers){
    df_traditional <- df %>%
      mutate_at(
        c(dv, viz_prep_final[["nodes"]][["id"]]),
        ~as.numeric(.) %>% scale(.)
      )
  }else{
    df_traditional <- df
  }


  traditional_drivers <- driver(
    df = df_traditional,
    dv = dv,
    ivs = viz_prep_final[["nodes"]][["id"]],
    subgroups = subgroups,
    labels = viz_prep_final[["nodes"]][["label"]] %>% unlist() %>% as.character(),
    engine = traditional_driver_engine
  )



  viz_prep_final[["nodes"]] <- viz_prep_final[["nodes"]] %>%
    left_join(
      impact_attributes %>% select(Variable, Total),
      by = join_by(id == Variable)
    ) %>%
    mutate(
      value = Total / 100
    ) %>%
    select(-Total)



  if(community_score_by == "sum"){

    community_nodes <- tibble(
      id = impact_community_by_sum[["community_name"]],
      value = impact_community_by_sum[["Total"]],
      label = id
    )

  }else if(community_score_by == "mean"){

    community_nodes <- tibble(
      id = impact_community_by_mean[["community_name"]],
      value = impact_community_by_mean[["Total"]],
      label = id
    )

  }


  community_nodes <- community_nodes %>% left_join(
      viz_prep_final[["nodes"]] %>%
        select(community_name, group, color) %>%
        distinct(),
      by = join_by(id == community_name)
    )



  visual_attribute <- visNetwork::visNetwork(
    nodes = viz_prep_final[["nodes"]],
    edges = viz_prep_final[["edges"]],
    height = "1500px",
    width = "1500px"
  ) %>%
    visNetwork::visIgraphLayout(layout = vs_layout) %>%
    visNetwork::visLayout(randomSeed = 1)


  visual_community <- visNetwork::visNetwork(
    nodes = community_nodes,
    edges = comunity_edges,
    height = "1500px",
    width = "1500px"
  ) %>%
    visNetwork::visIgraphLayout(layout = vs_layout) %>%
    visNetwork::visLayout(randomSeed = 1)


  results <- list(
    model = bn,
    impact_attributes = impact_attributes,
    attribute_community_contribution = attribute_community_contribution,
    impact_community_by_sum = impact_community_by_sum,
    impact_community_by_mean = impact_community_by_mean,
    nodes_attributes = viz_prep_final[["nodes"]],
    edges_attributes = viz_prep_final[["edges"]],
    nodes_community = community_nodes,
    edges_community = comunity_edges,
    visual_attribute = visual_attribute,
    visual_community = visual_community,
    traditional_drivers = traditional_drivers
  )


  return(results)

}
