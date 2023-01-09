##' pen_polygon
##'
##' A function that create a polygon object from coordinates
##'
##' @param coords a matrix of dim c(4, 2) containing the x and y
##'     coordinates of the 4 corners of the polygon in
##'     counterclockwise order
##' @param ID The id of the poly
##' @importFrom sp Polygons
##' @importFrom sp Polygon
##' @return A polygon object
pen_polygon <- function(coords, ID = "poly1") {

    ID <- as.character(ID)
    stopifnot(dim(coords) == c(4, 2))
    row.names(coords) <- NULL
    coords <- rbind(coords, coords[1,])
    poly <- Polygons(list(Polygon(coords)), ID = ID)

    if (poly@Polygons[[1]]@hole) {
        stop("coords should be in clockwise order")
    }
    return(poly)
}
##' make_grid
##'
##' a utility function to make a grid
##'
##' @param width The total width in number of grid subunits
##' @param height The total height in number of grid subunits
##' @param scale.x The size of the x units of the grid
##' @param scale.y The size of the y units of the grid
##' @param reference_pt The bottom left corner of the grid. A vector
##'     of length 2 with the x and y coordinate.
##' @return a matrix with points on a grid
make_grid <- function(width,
                      height,
                      scale.x = 1,
                      scale.y = 1,
                      reference_pt = c(0, 0)) {

    ## Check parameters
    stopifnot(length(width) == 1)
    stopifnot(length(height) == 1)
    stopifnot(width > 0 & height > 0)
    reference_pt <- as.numeric(reference_pt)
    stopifnot(length(reference_pt) == 2)
    stopifnot(all(!is.na(reference_pt)))

    ## Create a grid of the vertices of the boxes
    grid <- expand.grid(x = 0:width, y = 0:height)
    grid <- as.matrix.data.frame(grid)
    reference_pt <- matrix(rep(reference_pt, nrow(grid)), ncol = 2, byrow = TRUE)
    grid[,1] <- grid[,1] * scale.x
    grid[,2] <- grid[,2] * scale.y
    grid <- grid + reference_pt
    grid
}

##' grid_to_polygons
##'
##' Convert a grid to polygons
##'
##' @param grid A grid object
##' @importFrom sp SpatialPolygons
##' @importFrom sp SpatialPolygonsDataFrame
##' @return A grid of polygons
grid_to_polygons <- function(grid) {

    ## The indices of the boxes
    seq_width <- seq_len(length(unique(grid[,"x"])) -1)
    seq_height <- seq_len(length(unique(grid[,"y"])) -1)

    ## Convert these to rings
    polys <-  apply(expand.grid(seq_width, seq_height),
                    1,
                    function(i) {
                        c1 <- i[1] + ((max(seq_width) + 1) * (i[2] - 1))
                        c2 <- c1 + 1
                        c3 <- i[1] + ((max(seq_width) + 1) * (i[2]) + 1)
                        c4 <- c3 - 1
                        pen_polygon(grid[c(c1, c4, c3, c2),], paste0(i[1], "-", i[2]))
                    })
    ## The stitch them together in a spatial object
    polys <- SpatialPolygons(polys)
    df <- data.frame(ID = names(polys))
    row.names(df) <- names(polys)
    SpatialPolygonsDataFrame(polys, data = df)
}

##' pen_polygons
##'
##' A function to create a group of pens:
##'
##' @param width number of pens wide
##' @param height number of pens high
##' @param scale.x The width of each pen
##' @param scale.y The height of each pen
##' @param reference_pt The coordinate of the bottom left of the group
##'     of pens
##' @param IDS The ids of the pens
##' @return A group of pens
pen_polygons <- function(width,
                         height,
                         scale.x = 1,
                         scale.y = 1,
                         reference_pt = c(0, 0),
                         IDS = NULL) {

    grid <- make_grid(width,
                      height,
                      scale.x = scale.x,
                      scale.y = scale.y,
                      reference_pt)
    polys <- grid_to_polygons(grid)

    ## If you supply a vector of unique names you can crop the
    ## polygons object with it
    if (!is.null(IDS)) {
        stopifnot(length(polys) >= length(IDS))
        polys <- polys[seq_len(length(IDS)), ]
        polys@data$ID <- as.character(IDS)
    }
    return(polys)
}

##' farm_polygons
##'
##' Create the farm polygons
##'
##' @param result a result of a trajectory
##' @export
##' @return A polygon object
farm_polygons <- function(result = NULL) {
    ## Get the pens
    farm <- u0(node = TRUE)
    levels <- farm[, c("section", "pentype")]
    levels <- levels[!duplicated(levels), ]

    ## Specify the location of each section in the diagramme
    levels$ref_x <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 30, 30, 30,
                      30, 30, 30, 30, 30, 30, 30, 30, 57, 57, 57, 57,
                      57, 57, 57, 57, 57, 86, 86, 86, 86, 86, 86, 86,
                      86, 86, 0)
    levels$ref_y <- c(0, 1.5, 3.5, 5, 7, 9.5, 13, 15.5, 18, 20.5, 23,
                      25.5, 0, 2.5, 5, 7.5, 10, 12.5, 15, 17.5, 20,
                      22.5, 25.5, 0, 2.5, 5, 7.5, 10, 12.5, 15, 17.5,
                      20, 0, 2.5, 5, 7.5, 10, 12.5, 15, 17.5, 20, 29)

    ## Specify how many rows should each section be displayed in
    levels$nrow <- c(1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
                     2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
                     2, 2, 2, 2, 2, 2, 2, 2, 2, 2)

    ## Generate the polygons and give them labels
    farm <- do.call("rbind", lapply(seq_len(nrow(levels)), function(x) {
        section <- levels[x,]
        section_pens <- farm[farm$section == section$section & farm$pentype == section$pentype, ]
        pens <- pen_polygons(width = ceiling(nrow(section_pens) / section$nrow),
                             height = section$nrow,
                             reference_pt = c(section$ref_x, section$ref_y),
                             IDS = section_pens$node)
        pens@data$pentype <- section_pens$pentype
        pens@data$section <- section_pens$section
        pens
    }))

    if (is.null(result))
        return(farm)
    result <- clean_trajectory(result)
    stopifnot("MRSA_single_step_trajectory" %in% class(result))
    farm@data$I <- rowSums(result[, c("Isows1", "Isows2", "Isows3",
                                      "Igilts1", "Igilts2", "Igilts3",
                                      "Ipiglets1", "Ipiglets2", "Ipiglets3",
                                      "Igrowers1", "Igrowers2", "Igrowers3",
                                      "Ifinish1", "Ifinish2", "Ifinish3")])
    farm@data$S <- rowSums(result[, c("Ssows", "Sgilts", "Spiglets",
                                      "Sgrowers", "Sfinish")])
    farm@data$phi <- result$phi
    farm
}

##' plot.MRSA_single_step_trajectory
##'
##' @param x a trajectory for one step
##' @param xlim The limits of the x axis of the plot
##' @param col The colours of the background of the nodes
##' @param breaks The breaks in phi to select a colour for a node
##' @param ... Other arguments
##' @importFrom sp plot
##' @importFrom sp spsample
##' @importFrom graphics text
##' @method plot MRSA_single_step_trajectory
##' @return A plot
##' @export
plot.MRSA_single_step_trajectory <- function(x,
                                             xlim = c(-8, 100),
                                             col = c("#FF000000",
                                                     "#FF000019",
                                                     "#FF000032",
                                                     "#FF00004b",
                                                     "#FF000064",
                                                     "#FF00007d",
                                                     "#FF000096",
                                                     "#FF0000af",
                                                     "#FF0000c8",
                                                     "#FF0000e1",
                                                     "#FF0000fe"),
                                             breaks = c(0.000000,
                                                        6.310501,
                                                        18.707384,
                                                        31.389292,
                                                        44.337289,
                                                        58.628841,
                                                        74.981242,
                                                        93.845223,
                                                        119.361628,
                                                        175.595168),
                                             ...) {

    stopifnot(length(col) == length(breaks) + 1)
    time <- x$time[1]
    x <- farm_polygons(x)

    x@data$phi_cat <- as.numeric(as.character(cut(x@data$phi,
                                              breaks = breaks,
                                              include.lowest = TRUE,
                                              labels = seq_len(length(breaks) - 1))))
    x@data$phi_col <- col[x@data$phi_cat]
    plot(x, col = x$phi_col, xlim = xlim, main = paste(time, "days"))
    text(-2, 0.5,  adj = c(1,0), "Sow breeding")
    text(-2, 4,  adj = c(1,0), "Gilt breeding")
    text(-2, 7, adj = c(1,0), "Sow gestation")
    text(-2, 10, adj = c(1,0), "Gilt gestation")
    text(-2, 20, adj = c(1,0), "Farrowing")
    text(-2, 29.5, adj = c(1,0), "Gilt growing")
    text(28, 14.5, adj = c(1,0), "Growing")
    text(55, 14.5, adj = c(1,0), "Finishing")
    text(84, 14.5, adj = c(1,0), "Finishing")

    for (i in which(x$S > 0)) {
        plot(spsample(x[i, ],
                      x@data$S[i],
                      "random"),
             add = TRUE,
             pch = 20,
             cex = 1,
             col = "#0000FF50")
    }

    for(i in which(x$I > 0)) {
        plot(spsample(x[i, ],
                      x@data$I[i],
                      "random"),
             add = TRUE,
             pch = 20,
             cex = 1,
             col = "#FF0000FF")
    }
}

##' plot.MRSA_trajectory
##'
##' @param x a cleaned model trajectory. Something like the result of 'clean_trajectory(trajectory(run(model)))'
##' @param ... Other arguments
##' @method plot MRSA_trajectory
##' @examples
##' \dontrun{
##' library(LAMRSAControl)
##' model <- MRSA_model_4_parameter()
##' data(events)
##' model@events <- SimInf_events(model@events@E, model@events@N, events = events[events$time < 731,])
##' model@tspan <- as.double(1:730)
##' result <- clean_trajectory(trajectory(run(model)))
##' dir.create("movie")
##' plot(result, tspan = 600:605, path = "movie")
##'
##' ## You may now take these files and create a movie. If you have
##' ## ffmpeg on your machine you can do that like this:
##' ##     ## Combining a series of generated images into a video:
##' system("ffmpeg -start_number 600 -i movie/plots/plot%05d.png -r 10 -c:v libx264 -strict -2 -preset veryslow -pix_fmt yuv420p -vf scale=trunc\\(iw/2\\)*2:trunc\\(ih/2\\)*2 -f mp4 farm-indirect.mp4")
##' }
##' @return A series of plots
##' @export
plot.MRSA_trajectory <- function(x,
                                 tspan = 1:5,
                                 path = tempdir(), ...) {
    dir.create(file.path(path, "plots"))
    for(time in tspan) {
        file_path <- file.path(path, sprintf("plots/plot%05d.png", time))
        png(file_path, width = 1200, height = 500)
        plot(clean_trajectory(result[result$time == time, ]), ...)
        dev.off()
    }
}
