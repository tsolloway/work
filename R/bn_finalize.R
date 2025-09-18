#' bn_finalize
#' @description bn_finalize
#' @export
bn_finalize <- function(
    obj,
    df,
    dv,
    subgroups,
    dictionary = NULL,
    traditional_driver_engine = c("linear", "logistic"),
    community_score_by = c("sum", "mean"),
    node_label_type = c("both", "variable", "label"),
    manual_groups = NULL,
    node_size = NULL,
    standardize_traditional_drivers = FALSE,
    attribute_viz_label_var_only = TRUE,
    vs_layout = "layout_with_fr",
    vs_height = "100vh",
    vs_width = "100%",
    random_seed = 1,
    interactive_map_deliverable = TRUE,
    save_visuals = TRUE
){

  traditional_driver_engine <- match.arg(traditional_driver_engine)
  community_score_by <- match.arg(community_score_by)
  node_label_type <- match.arg(node_label_type)


  dictionary <- dictionary_from_named_object(dictionary)


  if("meta" %in% names(obj)){

    #logic for bn_tan return objects

    if(obj[["meta"]][["analysis"]] == "bn_model_single"){

      if(!is.null(manual_groups)){

        obj[["viz_prep"]] <- bn_to_netviz_prep(
          bn = obj,
          dictionary = dictionary,
          node_label_type = node_label_type,
          manual_groups = manual_groups,
          on_exit_detach_igraph = FALSE
        )

      }
    }

  }else{

    #logic for the bn_to_netviz_prep object

    if(all(c("nodes", "edges") %in% names(obj))){

      xnodes <- obj[["nodes"]]
      xedges <- obj[["edges"]]
      xivs <- obj[["nodes"]][["id"]] %>% unlist() %>% as.character() %>% setNames(NULL)


      black_list <- xivs %>%
        list(., .) %>%
        map_dfr(make_arcs)


      if("community_name" %in% names(xnodes) && is.null(manual_groups)){
        manual_groups <- xnodes
      }


      obj <- bn_tan(
        df = df,
        dv = dv,
        ivs = xivs,
        white_list = xedges %>% select(from, to),
        black_list = black_list,
        dictionary = dictionary,
        cross_battery_first = FALSE,
        suppress_bn_warning = TRUE,
        node_label_type = node_label_type,
        manual_groups = manual_groups,
        on_exit_detach_igraph = FALSE
      )

    }else{
      stop("Unknown object being passed to the obj parameter.")
    }
  }


  ###############################
  # Now modeling prep
  ###############################

  xnodes <- obj[["viz_prep"]][["nodes"]]
  xedges <- obj[["viz_prep"]][["edges"]]
  xivs <- obj[["viz_prep"]][["nodes"]][["id"]] %>% unlist() %>% setNames(NULL)


  if("community_name" %in% names(xnodes) && is.null(manual_groups)){
    manual_groups <- xnodes
  }


  black_list <- xivs %>%
    list(., .) %>%
    map_dfr(make_arcs)



  ###############################
  # Subgroup models
  ###############################

  subgroup_models <- subgroups %>% map(
    ~bn_tan(
      df = df %>%
        filter_at(
          .x, all_vars(equals(., 1))
        ),
      dv = dv,
      ivs = xivs,
      white_list = xedges %>% select(from, to),
      black_list = black_list,
      dictionary = dictionary,
      cross_battery_first = FALSE,
      compare_to_niave = TRUE,
      suppress_bn_warning = TRUE,
      on_exit_detach_igraph = FALSE
    )
  ) %>%
    setNames(subgroups) %>%
    suppressWarnings() %>%
    suppressMessages()



  ###############################
  # Impacts attributes by MI
  ###############################

  results_raw_list <- subgroup_models %>%
    map(pluck, "arcs")


  impact_attributes_by_mi <- results_raw_list %>%
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
      xnodes %>% select(id, community_name),
      by = join_by(Variable == id)
    ) %>%
    relocate(community_name, .after = Variable) %>%
    as_tibble() %>%
    setNames(., names(.) %>% trimws()) %>%
    arrange(-Total)



  ###############################
  # Impacts community by MI
  ###############################

  impact_community_by_mi_sum <- impact_attributes_by_mi %>%
    select_if(stringr::str_detect(names(.), "MI|community_name")) %>%
    group_by(community_name) %>%
    summarise_if(is.numeric, sum) %>%
    mutate_if(is.numeric, function(y) y %>% map(~(.x/mean(abs(y)))*100) %>% as.numeric()) %>%
    setNames(., names(.) %>% gsub(" MI", "", .)) %>%
    arrange(-Total)


  impact_community_by_mi_mean <- impact_attributes_by_mi %>%
    select_if(stringr::str_detect(names(.), "MI|community_name")) %>%
    group_by(community_name) %>%
    summarise_if(is.numeric, mean) %>%
    mutate_if(is.numeric, function(y) y %>% map(~(.x/mean(abs(y)))*100) %>% as.numeric()) %>%
    setNames(., names(.) %>% gsub(" MI", "", .)) %>%
    arrange(-Total)



  attribute_community_contribution_by_mi <- results_raw_list %>%
    map(pluck, "ivs") %>%
    imap(
      ~rbind(
        .x %>% select(from, mi) %>% setNames(c("Variable", "mi")),
        .x %>% select(to, mi) %>% setNames(c("Variable", "mi"))
      ) %>%
        left_join(
          impact_attributes_by_mi %>% select(Variable, community_name),
          by = join_by(Variable == Variable)
        ) %>%
        group_by(community_name, Variable) %>%
        summarise_at("mi", ~sum(.)/2) %>%
        group_by(community_name) %>%
        mutate(
          {{.y}} := mi / sum(mi)
        ) %>%
        ungroup() %>%
        select(-mi)
    ) %>%
    plyr::join_all(by = c("Variable", "community_name")) %>%
    arrange(community_name, -Total)



  ###############################
  # Edges
  ###############################

  attribute_edges <- results_raw_list %>%
    map(pluck, "ivs") %>%
    imap(
      ~.x %>%
        select(from, to, mi, pval) %>%
        mutate({{.y}} := (mi / mean(abs(mi))) * 100 ) %>%
        rename(!!glue("{.y} MI") := mi) %>%
        rename(!!glue("{.y} P Value") := "pval")
    ) %>%
    plyr::join_all(by = c("from", "to"), type = "full") %>%
    arrange(from, to)


  comunity_edges <- attribute_edges %>%
    left_join(
      impact_attributes_by_mi %>% select(Variable, community_name),
      by = join_by(from == Variable)
    ) %>%
    rename(from_community_name = community_name) %>%
    left_join(
      impact_attributes_by_mi %>% select(Variable, community_name),
      by = join_by(to == Variable)
    ) %>%
    rename(to_community_name = community_name) %>%
    filter(from_community_name != to_community_name) %>%
    relocate(from_community_name, to_community_name, .after = to)




  #####################################
  # Traditional Drivers
  #####################################

  df_traditional <- df %>%
    mutate_if(
      is.factor,
      ~as.character(.) %>% as.numeric(.)
    )

  if(standardize_traditional_drivers){
    df_traditional <- df_traditional %>%
      mutate_at(c(dv, xivs), scale)
  }


  traditional_drivers <- drivers(
    df = df_traditional,
    dv = dv,
    ivs = xivs,
    subgroups = subgroups,
    labels = dictionary,
    engine = traditional_driver_engine,
    shift_percentage = 0.05,
    label_width = "auto",
    write = TRUE
  )



  #####################################
  # Attribute Visuals
  #####################################


  if(is.null(node_size)){

    obj[["viz_prep"]][["nodes"]] <- obj[["viz_prep"]][["nodes"]] %>%
      arrange(id) %>%
      left_join(
        impact_attributes_by_mi %>% select(Variable, Total),
        by = join_by(id == Variable)
      ) %>%
      mutate(
        value = Total / 100
      ) %>%
      select(-Total)

  }else{

    obj[["viz_prep"]][["nodes"]] <- obj[["viz_prep"]][["nodes"]] %>%
      mutate(
        value = node_size
      )

  }


  visual_attribute_no_layout <- visNetwork::visNetwork(
    nodes = obj[["viz_prep"]][["nodes"]] %>% arrange(id),
    edges = obj[["viz_prep"]][["edges"]],
    height = vs_height,
    width = vs_width
  ) %>%
    visNetwork::visLayout(randomSeed = random_seed)


  visual_attribute_physics <- visual_attribute_no_layout %>%
    visNetwork::visPhysics(
      solver = "barnesHut",
      stabilization = TRUE,
      barnesHut = list(
        # Decrease gravitationalConstant for stronger repulsion
        gravitationalConstant = -9000,
        # Lower centralGravity to reduce pull to the center
        centralGravity = .2,
        # Increase avoidOverlap to prevent nodes from physically overlapping
        avoidOverlap = 1
      )
    )


  visual_attribute_layout <- visual_attribute_no_layout %>%
    visNetwork::visIgraphLayout(layout = vs_layout)


  visual_attribute_hierarchical <- visual_attribute_no_layout %>%
    visNetwork::visHierarchicalLayout()


  visual_attribute_hierarchical[["x"]][["nodes"]][["label"]] <- visual_attribute_hierarchical[["x"]][["nodes"]][["label"]] %>%
    strsplit(" - ") %>%
    map_chr(head(1))



  #####################################
  # Community Visuals
  #####################################

  if(community_score_by == "sum"){

    community_nodes <- tibble(
      id = impact_community_by_mi_sum[["community_name"]],
      value = impact_community_by_mi_sum[["Total"]],
      label = id
    )

  }else if(community_score_by == "mean"){

    community_nodes <- tibble(
      id = impact_community_by_mi_mean[["community_name"]],
      value = impact_community_by_mi_mean[["Total"]],
      label = id
    )

  }


  community_nodes <- community_nodes %>% left_join(
    obj[["viz_prep"]][["nodes"]] %>%
      select(community_name, group, color) %>%
      distinct(),
    by = join_by(id == community_name)
  )



  visual_community_no_layout <- visNetwork::visNetwork(
    nodes = community_nodes,
    edges =   comunity_edges %>%
      select(c("from_community_name", "to_community_name", "Total MI")) %>%
      rename(
        from = from_community_name,
        to = to_community_name,
        value = "Total MI"
      ),
    height = vs_height,
    width = vs_width
  ) %>%
    visNetwork::visLayout(randomSeed = random_seed)



  visual_community_physics <- visual_community_no_layout %>%
    visNetwork::visPhysics(
      solver = "barnesHut",
      stabilization = TRUE,
      barnesHut = list(
        # Decrease gravitationalConstant for stronger repulsion
        gravitationalConstant = -9000,
        # Lower centralGravity to reduce pull to the center
        centralGravity = .2,
        # Increase avoidOverlap to prevent nodes from physically overlapping
        avoidOverlap = 1
      )
    )


  visual_community_layout <- visual_community_no_layout %>%
    visNetwork::visIgraphLayout(layout = vs_layout)


  visual_community_hierarchical <- visual_community_no_layout %>%
    visNetwork::visHierarchicalLayout()



  #####################################
  # Interactive Visuals
  #####################################


  if(interactive_map_deliverable){

    df_key <- visual_attribute_no_layout[["x"]][["nodes"]] %>%
      arrange(group) %>%
      select(community_name, color) %>%
      distinct() %>%
      rename(label = community_name) %>%
      mutate(shape = "dot", size = 20)


    viz_interactive_plan <- function(viz, add_key = TRUE){

      if(add_key){
        viz <- viz %>%
          visNetwork::visLegend(
            useGroups = FALSE,
            addNodes = df_key
          )
      }

      viz %>% bn_visNetwork_deliverable_interactivity()
    }


    visual_attribute_no_layout <- visual_attribute_no_layout %>% viz_interactive_plan()
    visual_attribute_physics <- visual_attribute_physics %>% viz_interactive_plan()
    visual_attribute_layout <- visual_attribute_layout %>% viz_interactive_plan()
    visual_attribute_hierarchical <- visual_attribute_hierarchical %>% viz_interactive_plan()

    visual_community_no_layout <- visual_community_no_layout %>% viz_interactive_plan(add_key = FALSE)
    visual_community_physics <- visual_community_physics %>% viz_interactive_plan(add_key = FALSE)
    visual_community_layout <- visual_community_layout %>% viz_interactive_plan(add_key = FALSE)
    visual_community_hierarchical <- visual_community_hierarchical %>% viz_interactive_plan(add_key = FALSE)
  }



  #####################################
  # Save Visuals
  #####################################

  if(save_visuals){
    visual_attribute_no_layout %>% visNetwork::visSave(file = "Attribute_Network_No_Layout.html", selfcontained = TRUE)
    visual_attribute_physics %>% visNetwork::visSave(file = "Attribute_Network_Physics.html", selfcontained = TRUE)
    visual_attribute_layout %>% visNetwork::visSave(file = "Attribute_Network_Layout.html", selfcontained = TRUE)
    visual_attribute_hierarchical %>% visNetwork::visSave(file = "Attribute_Network_Hierachical.html", selfcontained = TRUE)

    visual_community_no_layout %>% visNetwork::visSave(file = "Community_Network_No_Layout.html", selfcontained = TRUE)
    visual_community_physics %>% visNetwork::visSave(file = "Community_Network_Physics.html", selfcontained = TRUE)
    visual_community_layout %>% visNetwork::visSave(file = "Community_Network_Layout.html", selfcontained = TRUE)
    visual_community_hierarchical %>% visNetwork::visSave(file = "Community_Network_Hierachical.html", selfcontained = TRUE)
  }




  #####################################
  # Send analytics to results output
  #####################################

  final_excel_clean <- function(x){
    x %>%
      mutate_if(is.numeric, round, 4) %>%
      setNames(
        .,
        names(.) %>%
          gsub("_", " ", .) %>%
          stringr::str_to_title()
      )
  }


  results <- list(
    bn = obj,
    bn_subgroups = subgroup_models,
    impact_attributes_by_mi = impact_attributes_by_mi %>% final_excel_clean(),
    attribute_community_contribution_by_mi = attribute_community_contribution_by_mi %>% final_excel_clean(),
    impact_community_by_mi_sum = impact_community_by_mi_sum %>% final_excel_clean(),
    impact_community_by_mi_mean = impact_community_by_mi_mean %>% final_excel_clean(),
    traditional_drivers = traditional_drivers,
    attribute_edges = attribute_edges %>% final_excel_clean(),
    comunity_edges = comunity_edges %>% final_excel_clean(),

    viz_prep_attributes = obj[["viz_prep"]],
    viz_prep_community = list(
      nodes = community_nodes,
      edges = comunity_edges %>%
        select(c("from_community_name", "to_community_name", "Total MI")) %>%
        rename(
          from = from_community_name,
          to = to_community_name,
          value = "Total MI"
        )
    ),

    visual_attribute = list(
      visual_attribute_no_layout = visual_attribute_no_layout,
      visual_attribute_physics = visual_attribute_physics,
      visual_attribute_layout = visual_attribute_layout,
      visual_attribute_hierarchical = visual_attribute_hierarchical
    ),

    visual_community = list(
      visual_community_no_layout = visual_community_no_layout,
      visual_community_physics = visual_community_physics,
      visual_community_layout = visual_community_layout,
      visual_community_hierarchical = visual_community_hierarchical
    )
  )



  detach_igraph()


  return(results)

}
