#' Saisie des points de calibration de mortalité
#'
#' @param an1 Année n+1
#' @param an0 Année n (par défaut an1 - 1 an)
#'
#' @return table x, y, type (sec ou disparu)
#' @export
#'

spot_saisie_morts <- function(an1 = 2023, an0 = NULL){


  ui <- shiny::fluidPage(

    shiny::fluidRow(style = "display:flex;",
                    shiny::tags$style(".leaflet-interactive {cursor: none;}"),

                    shiny::selectInput("ug", "UG", choices = unique(sort(dc("shp")$id)) %>% util.id_simple(),
                                       selected = dc("shp") %>%
                                         dplyr::mutate(idd = util.id_simple(id),
                                                       area = sf::st_area(.)) %>%
                                                                   dplyr::filter(area == min(area)) %>%
                                                                   dplyr::slice(1) %>%
                                                                   dplyr::pull(idd)),

                    shiny::selectInput("type", "nature", choices = c("sec", "disparu"), selected = "sec"),

                    shiny::tags$span(style = "position: absolute; right: 5px; top: 5px;",
                                     shiny::actionButton("ok", "OK", icon = shiny::icon("close", verify_fa = FALSE),
                                                         style = "background: red;")
                    )

    ),
    # shiny::htmlOutput("map")
    fluidRow(style  = "display:flex;",
             leaflet::leafletOutput("map0"),
             leaflet::leafletOutput("map1")
    ),

    tableOutput("table")
  )


  server <- function(input, output, session) {


    r <- shiny::reactiveValues(
      tab = data.frame(
        x = 0,
        y = 0,
        type = "",
        UG = "",
        stringsAsFactors = FALSE
      ) %>% dplyr::slice(-1),

      update = 0
    )

    if(is.null(an0)) an0 <- an1-1

    shp <- dc("shp") %>% dplyr::mutate(ID = util.id_simple(id))

    spot1 <- uRast("spot", an1)
    spot0 <- uRast("spot", an0)

    spot <- c(spot0, spot1)


    # observers -----------------------------------------



    shiny::observeEvent(input$ug, ignoreInit = FALSE, {


      ug_v <- shp %>% dplyr::filter(ID == input$ug)

      spot_ug <- spot %>%
        terra::crop(ug_v  %>% as("SpatVector") %>% terra::buffer(dc("buffer"))
        )

      for(i in 0:1){

        fun <- ifelse(i == 0, min, max)

        r[[paste0("m", i)]] <- leaflet() %>%
          addPolygons(data = ug_v %>% sf::st_transform(4326),
                      weight = 1, opacity = 1, fillOpacity = 0, color = "blue") %>%

          leafem::addRasterRGB(spot_ug[[which(terra::time(spot) == fun(terra::time(spot)))]] %>%
                                 util_spat2rast(),
                               quantiles = c(0.01, .9),
                               r = 1, g = 2, b = 3)
      }

      r$m0 <- r$m0 %>%
        htmlwidgets::onRender(
          "function(el,x){
                    this.on('mousemove', function(e) {
                        var lat = e.latlng.lat;
                        var lng = e.latlng.lng;
                        var coord = [lat, lng];
                        Shiny.onInputChange('hover_coordinates', coord)
                    });
                    this.on('mouseout', function(e) {
                        Shiny.onInputChange('hover_coordinates', null)
                    })
                }"
        )
    })

    output$map1 <- leaflet::renderLeaflet({
      # leafsync::sync(r$m0, r$m1)
      r$m1
    })

    output$map0 <- leaflet::renderLeaflet({
      # leafsync::sync(r$m0, r$m1)
      r$m0
    })

    # -----------------------------------------------

    shiny::observeEvent(input$map0_click,{

      if(!is.null(input$map0_marker_click)){

        req(
          (input$map0_marker_click$lng == input$map0_marker_click$lng) &
           (input$map0_marker_click$lat == input$map0_marker_click$lat)
        )
      }

      new <- data.frame(x = input$map0_click$lng,
                        y = input$map0_click$lat,
                        type = input$type,
                        UG = input$ug,
                        stringsAsFactors = FALSE)

      shiny::isolate(r$tab <- rbind(r$tab, new))

      r$update <- r$update + 1

      shiny::showNotification("Point ajouté")
    })

        # -----------------------------------------

    shiny::observeEvent(r$update,{


      for(m in c("map0", "map1")){

        leaflet::leafletProxy(m) %>%
          leaflet::clearGroup("points") %>%
          leaflet::addCircleMarkers(r$tab$x, r$tab$y,color = r$tab %>% dplyr::mutate(col = ifelse(type == "sec", "red", "orange")) %>% dplyr::pull(col),
                                    group = "points")
      }

    })


    shiny::observeEvent(input$map0_marker_click,{

      pt <- input$map0_marker_click

      tab <- r$tab %>% dplyr::mutate(id = row.names(.))

      idr <- tab %>% dplyr::filter(round(x, 8) == round(pt$lng, 8) & round(y, 8) == round(pt$lat, 8)) %>%
        dplyr::pull(id)

      r$tab <- tab %>% dplyr::filter(id != idr) %>% dplyr::select(-id)


      r$update <- r$update + 1

      shiny::showNotification("Point supprimé")

    })

    observeEvent(input$maj, ignoreInit = TRUE, {
      r$update <- r$update+1
    })

    observeEvent(input$ug, ignoreInit = TRUE, {
      r$update <- r$update+1
    })


    # sync---------------------

    observeEvent(input$map0_bounds,{

      b <- input$map0_bounds

      leaflet::leafletProxy("map1") %>%
        leaflet::fitBounds(b$west, b$north, b$east, b$south)
    })

    observeEvent(input$map0_center,{
      req( input$map0_zoom)
      b <- input$map0_center

      leaflet::leafletProxy("map1") %>%
        leaflet::setView(b$lng, b$lat, input$map0_zoom)
    })


    observeEvent(input$map0_zoom,{
      req( input$map0_center)
      b <- input$map0_center

      leaflet::leafletProxy("map1") %>%
        leaflet::setView(b$lng, b$lat, input$map0_zoom)
    })

    observeEvent(input$hover_coordinates,{

      req(input$hover_coordinates)

      pos <- input$hover_coordinates

      leaflet::leafletProxy("map1") %>%
        leaflet::removeControl("curseur") %>%
        leaflet::addCircleMarkers(
          fillOpacity = 0,
          lng = pos[2], lat = pos[1],
          layerId = "curseur")

      leaflet::leafletProxy("map0") %>%
        leaflet::removeControl("curseur") %>%
        leaflet::addCircleMarkers(
          fillOpacity = 0,
          lng = pos[2], lat = pos[1],
          layerId = "curseur")

    })

    output$table <- shiny::renderTable(r$tab)

    shiny::observeEvent(input$ok, shiny::stopApp(r$tab))

  }

  shiny::runGadget(shiny::shinyApp(ui, server), viewer = shiny::browserViewer())


}
