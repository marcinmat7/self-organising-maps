# Self organising maps - simple examples

library("plotly")
library("dplyr")
library("kohonen")

# Example 1
    
    set.seed(250)
    xs <- runif(n = 10000, min = 0, max = 10)
    ys <- runif(n = 10000, min = 0, max = 10)
    df <- data.frame(xs, ys)
    
    map_dim <- 5
    g <- somgrid(xdim = map_dim, ydim = map_dim, topo = "rectangular")
    
    df2 <- data.frame(xs, ys)
    df2 <- scale(df2, center = FALSE, scale = FALSE)
    
    map <- som(df2,
               grid = g,
               alpha = c(0.05, 0.01),
               rlen = 1000)
    
    map1 <- data.frame(map$grid$pts)
    df3 <- map$codes
    df3 <- data.frame(df3)
    
    adj_points <- list()
    
    i <- 1
    for(i in 1:nrow(map1)) {
        
        x_tmp <- map1[i, 1]
        y_tmp <- map1[i, 2]
        
        poss1 <- c(max(x_tmp - 1, 1), y_tmp)
        poss2 <- c(min(x_tmp + 1, map_dim), y_tmp)
        
        poss3 <- c(x_tmp, max(y_tmp - 1, 1))
        poss4 <- c(x_tmp, min(y_tmp + 1, map_dim))
        
        vec_tmp <- c(which(paste0(map1$x, "_", map1$y) == paste0(poss1[1], "_", poss1[2])),
                     which(paste0(map1$x, "_", map1$y) == paste0(poss2[1], "_", poss2[2])),
                     which(paste0(map1$x, "_", map1$y) == paste0(poss3[1], "_", poss3[2])),
                     which(paste0(map1$x, "_", map1$y) == paste0(poss4[1], "_", poss4[2])))
        
        adj_points[[i]] <- vec_tmp   
        
    }
    
    df$zs <- 1
    df3$zs <- 1
    fig <- plot_ly(data = df, x = ~xs, y = ~ys, z = ~zs, 
                   mode = "markers",
                   type = "scatter3d",
                   marker = list(size = 1, color = "red")) %>%
        
        add_trace(
            data = df3, 
            x = ~xs, 
            y = ~ys, 
            z = ~zs, 
            mode = "markers", 
            type = "scatter3d", 
            marker = list(size = 3, color = "blue"))
    
    for(i in 1:length(adj_points)) {
        fig <- fig %>%  
            add_trace(
                data = df3[c(i, adj_points[[i]][1]), ],
                x = ~xs,
                y = ~ys,
                z = ~zs,
                mode = "lines",
                type = "scatter3d",
                # color = "yellow",
                line = list(size = 1, color = "blue", legend = NULL), legend = NULL) %>%  
            add_trace(
                data = df3[c(i, adj_points[[i]][2]), ],
                x = ~xs,
                y = ~ys,
                z = ~zs,
                mode = "lines",
                type = "scatter3d",
                # color = "yellow",
                line = list(size = 1, color = "blue", legend = NULL)) %>%  
            add_trace(
                data = df3[c(i, adj_points[[i]][3]), ],
                x = ~xs,
                y = ~ys,
                z = ~zs,
                mode = "lines",
                type = "scatter3d",
                # color = "yellow",
                line = list(size = 1, color = "blue", legend = NULL)) %>%  
            add_trace(
                data = df3[c(i, adj_points[[i]][4]), ],
                x = ~xs,
                y = ~ys,
                z = ~zs,
                mode = "lines",
                type = "scatter3d",
                # color = "yellow",
                line = list(size = 1, color = "blue", legend = NULL)) %>%
            layout(
                showlegend = F, 
                legend = list(orientation = 'h')
            )
    }
    
    fig



# Example 2
    
    set.seed(250)
    x1 <- rnorm(n = 500, mean = 1, sd = .4)
    x2 <- rnorm(n = 500, mean = 6, sd = .4)
    x3 <- rnorm(n = 2000, mean = 2, sd = .4)
    
    y1 <- rnorm(n = 500, mean = 1, sd = .4)
    y2 <- rnorm(n = 500, mean = 4, sd = .4)
    y3 <- rnorm(n = 2000, mean = 5, sd = .4)
    
    xs <- c(x1, x2, x3)
    ys <- c(y1, y2, y3)
    plot(xs, ys)
    df <- data.frame(xs, ys)
    
    set.seed(123)
    map_dim <- 5
    g <- somgrid(xdim = map_dim, ydim = map_dim, topo = "rectangular")
    
    df2 <- data.frame(xs, ys)
    df2 <- scale(df2, center = FALSE, scale = FALSE)
    df2
    
    map <- som(df2,
               grid = g,
               alpha = c(0.05, 0.01),
               # radius = 0.7, 
               rlen = 1000)
    
    map1 <- map$grid$pts
    map1 <- data.frame(map1)
    df3 <- map$codes
    df3 <- data.frame(df3)
    
    adj_points <- list()
    
    i <- 1
    for(i in 1:nrow(map1)) {
        print(i)
        x_tmp <- map1[i, 1]
        y_tmp <- map1[i, 2]
        
        poss1 <- c(max(x_tmp - 1, 1), y_tmp)
        poss2 <- c(min(x_tmp + 1, map_dim), y_tmp)
        
        poss3 <- c(x_tmp, max(y_tmp - 1, 1))
        poss4 <- c(x_tmp, min(y_tmp + 1, map_dim))
        
        vec_tmp <- c(which(paste0(map1$x, "_", map1$y) == paste0(poss1[1], "_", poss1[2])),
                     which(paste0(map1$x, "_", map1$y) == paste0(poss2[1], "_", poss2[2])),
                     which(paste0(map1$x, "_", map1$y) == paste0(poss3[1], "_", poss3[2])),
                     which(paste0(map1$x, "_", map1$y) == paste0(poss4[1], "_", poss4[2])))
        
        adj_points[[i]] <- vec_tmp   
        
        
    }
    
    df$zs <- 1
    df3$zs <- 1
    fig <- plot_ly(data = df, x = ~xs, y = ~ys, z = ~zs, 
                   mode = "markers",
                   type = "scatter3d",
                   marker = list(size = 1, color = "red")) %>%
        
        add_trace(
            data = df3, 
            x = ~xs, 
            y = ~ys, 
            z = ~zs, 
            mode = "markers", 
            type = "scatter3d", 
            marker = list(size = 3, color = "blue"))
    
    for(i in 1:length(adj_points)) {
        fig <- fig %>%  
            add_trace(
                data = df3[c(i, adj_points[[i]][1]), ],
                x = ~xs,
                y = ~ys,
                z = ~zs,
                mode = "lines",
                type = "scatter3d",
                # color = "yellow",
                line = list(size = 1, color = "blue", legend = NULL), legend = NULL) %>%  
            add_trace(
                data = df3[c(i, adj_points[[i]][2]), ],
                x = ~xs,
                y = ~ys,
                z = ~zs,
                mode = "lines",
                type = "scatter3d",
                # color = "yellow",
                line = list(size = 1, color = "blue", legend = NULL)) %>%  
            add_trace(
                data = df3[c(i, adj_points[[i]][3]), ],
                x = ~xs,
                y = ~ys,
                z = ~zs,
                mode = "lines",
                type = "scatter3d",
                # color = "yellow",
                line = list(size = 1, color = "blue", legend = NULL)) %>%  
            add_trace(
                data = df3[c(i, adj_points[[i]][4]), ],
                x = ~xs,
                y = ~ys,
                z = ~zs,
                mode = "lines",
                type = "scatter3d",
                # color = "yellow",
                line = list(size = 1, color = "blue", legend = NULL)) %>%
            layout(
                showlegend = F, 
                legend = list(orientation = 'h')
            )
    }
    fig
    


# Example 3
    
    x1 <- rnorm(n = 500, mean = 1, sd = .4)
    x2 <- rnorm(n = 500, mean = 8, sd = .4)
    x3 <- rnorm(n = 2000, mean = 2, sd = .4)
    
    y1 <- rnorm(n = 500, mean = 1, sd = .4)
    y2 <- rnorm(n = 500, mean = 4, sd = .4)
    y3 <- rnorm(n = 2000, mean = 5, sd = .4)
    
    xs <- c(x1, x2, x3)
    ys <- c(y1, y2, y3)
    plot(xs, ys)
    df <- data.frame(xs, ys)
    
    set.seed(123)
    map_dim <- 20
    g <- somgrid(xdim = map_dim, ydim = map_dim, topo = "rectangular")
    
    df2 <- data.frame(xs, ys)
    df2 <- scale(df2, center = FALSE, scale = FALSE)
    df2
    
    map <- som(df2,
               grid = g,
               alpha = c(0.05, 0.01),
               # radius = 0.7, 
               rlen = 1000)
    
    coolBlueHotRed <- function(n, alpha = 1) {rainbow(n, end=4/6, alpha=alpha)[n:1]}
    plot(map, type="count")


# Example 4 (1/4 sphere)
    
    set.seed(500)
    phi <- runif(n = 10000, min = 0, max = pi)
    theta <- runif(n = 10000, min = 0, max = pi / 2)
    r <- runif(n = 10000, min = 0.98, max = 1.02)
    
    
    xs = r * sin(phi) * cos(theta)
    ys = r * sin(phi) * sin(theta)
    zs = r * cos(phi)
    
    
    df <- data.frame(xs, ys, zs)
    df$points <- "orig"
    
    map_dim <- 10
    g <- somgrid(xdim = map_dim, ydim = map_dim, 
                 # topo = "hexagonal",
                 toroidal = FALSE)
    
    df2 <- data.frame(xs, ys, zs)
    somgrid()
    df2 <- scale(df2, center = FALSE, scale = FALSE)
    df2
    map <- som(df2,
               grid = g,
               # alpha = c(0),
               # radius = 0.7, 
               rlen = 100)
    map$distances
    df3 <- map$codes
    df3 <- data.frame(df3)
    
    df3$points <- "map"
    
    map1 <- map$grid$pts
    map1 <- data.frame(map1)
    # head(map1)
    adj_points <- list()
    
    i <- 1
    for(i in 1:nrow(map1)) {
        print(i)
        x_tmp <- map1[i, 1]
        y_tmp <- map1[i, 2]
        
        poss1 <- c(max(x_tmp - 1, 1), y_tmp)
        poss2 <- c(min(x_tmp + 1, map_dim), y_tmp)
        
        poss3 <- c(x_tmp, max(y_tmp - 1, 1))
        poss4 <- c(x_tmp, min(y_tmp + 1, map_dim))
        
        vec_tmp <- c(which(paste0(map1$x, "_", map1$y) == paste0(poss1[1], "_", poss1[2])),
                     which(paste0(map1$x, "_", map1$y) == paste0(poss2[1], "_", poss2[2])),
                     which(paste0(map1$x, "_", map1$y) == paste0(poss3[1], "_", poss3[2])),
                     which(paste0(map1$x, "_", map1$y) == paste0(poss4[1], "_", poss4[2])))
        
        adj_points[[i]] <- vec_tmp   
        
        
    }
    
    fig <- plot_ly(data = df, x = ~xs, y = ~ys, z = ~zs, 
                   mode = "markers",
                   type = "scatter3d",
                   marker = list(size = 1, color = "red")) %>%
        
        add_trace(
            data = df3, 
            x = ~xs, 
            y = ~ys, 
            z = ~zs, 
            mode = "markers", 
            type = "scatter3d", 
            marker = list(size = 3, color = "blue"))
    
    for(i in 1:length(adj_points)) {
        fig <- fig %>%  
            add_trace(
                data = df3[c(i, adj_points[[i]][1]), ],
                x = ~xs,
                y = ~ys,
                z = ~zs,
                mode = "lines",
                type = "scatter3d",
                # color = "yellow",
                line = list(size = 1, color = "blue")) %>%  
            add_trace(
                data = df3[c(i, adj_points[[i]][2]), ],
                x = ~xs,
                y = ~ys,
                z = ~zs,
                mode = "lines",
                type = "scatter3d",
                # color = "yellow",
                line = list(size = 1, color = "blue")) %>%  
            add_trace(
                data = df3[c(i, adj_points[[i]][3]), ],
                x = ~xs,
                y = ~ys,
                z = ~zs,
                mode = "lines",
                type = "scatter3d",
                # color = "yellow",
                line = list(size = 1, color = "blue")) %>%  
            add_trace(
                data = df3[c(i, adj_points[[i]][4]), ],
                x = ~xs,
                y = ~ys,
                z = ~zs,
                mode = "lines",
                type = "scatter3d",
                # color = "yellow",
                line = list(size = 1, color = "blue"))
        
    }
    
    fig <- fig %>%  layout(
        showlegend = F, 
        legend = list(orientation = 'h'))
    fig



# Example 5
    
    x1 <- runif(n = 5000, min = 0, max = 10)
    x2 <- runif(n = 5000, min = 0, max = 10)
    x3 <- runif(n = 5000, min = 0, max = 10)
    
    x1 <- c(runif(n = 5000, min = 0, max = 3), runif(n = 5000, min = 7, max = 10))
    x1 <- sample(x1)
    x2 <- c(runif(n = 5000, min = 0, max = 3), runif(n = 5000, min = 7, max = 10))
    x2 <- sample(x2)
    x3 <- c(runif(n = 5000, min = 0, max = 3), runif(n = 5000, min = 7, max = 10))
    x3 <- sample(x3)
    
    df <- data.frame(x1, x2, x3)
    
    ind1 <- df$x1 < 5 & df$x2 < 5 & df$x3 < 5
    ind2 <- df$x1 > 5 & df$x2 > 5 & df$x3 < 5
    ind3 <- df$x1 > 5 & df$x2 < 5 & df$x3 > 5
    ind4 <- df$x1 < 5 & df$x2 > 5 & df$x3 > 5
    ind <- ind1 | ind2 | ind3 | ind4
    
    df$val <- sample(x = c(0, 1), size = nrow(df), replace = T, prob = c(0.9, 0.1))
    df$val[ind] <- sample(x = c(0, 1), size = sum(ind), replace = T, prob = c(0.1, 0.9))
    
    map_dim <- 10
    g <- somgrid(xdim = map_dim, ydim = map_dim, 
                 # topo = "hexagonal",
                 toroidal = FALSE)
    
    ################
    xs = x1
    ys = x2
    zs = x3
    df <- data.frame(xs = x1, ys = x2, zs = x3)
    df$points <- "orig"
    
    map_dim <- 10
    g <- somgrid(xdim = map_dim, ydim = map_dim, 
                 # topo = "hexagonal",
                 toroidal = FALSE)
    
    df2 <- data.frame(xs, ys, zs)
    somgrid()
    df2 <- scale(df2, center = FALSE, scale = FALSE)
    df2
    map <- som(df2,
               grid = g,
               # alpha = c(0),
               # radius = 0.7, 
               rlen = 100)
    map$distances
    df3 <- map$codes
    df3 <- data.frame(df3)
    
    df3$points <- "map"
    
    map1 <- map$grid$pts
    map1 <- data.frame(map1)
    head(map1)
    adj_points <- list()
    
    i <- 1
    for(i in 1:nrow(map1)) {
        print(i)
        x_tmp <- map1[i, 1]
        y_tmp <- map1[i, 2]
        
        poss1 <- c(max(x_tmp - 1, 1), y_tmp)
        poss2 <- c(min(x_tmp + 1, map_dim), y_tmp)
        
        poss3 <- c(x_tmp, max(y_tmp - 1, 1))
        poss4 <- c(x_tmp, min(y_tmp + 1, map_dim))
        
        vec_tmp <- c(which(paste0(map1$x, "_", map1$y) == paste0(poss1[1], "_", poss1[2])),
                     which(paste0(map1$x, "_", map1$y) == paste0(poss2[1], "_", poss2[2])),
                     which(paste0(map1$x, "_", map1$y) == paste0(poss3[1], "_", poss3[2])),
                     which(paste0(map1$x, "_", map1$y) == paste0(poss4[1], "_", poss4[2])))
        
        adj_points[[i]] <- vec_tmp   
        
        
    }
    
    
    fig <- plot_ly(data = df, x = ~xs, y = ~ys, z = ~zs, 
                   mode = "markers",
                   type = "scatter3d",
                   marker = list(size = 1, color = "red")) %>%
        
        add_trace(
            data = df3, 
            x = ~xs, 
            y = ~ys, 
            z = ~zs, 
            mode = "markers", 
            type = "scatter3d", 
            marker = list(size = 3, color = "blue"))
    
    for(i in 1:length(adj_points)) {
        fig <- fig %>%  
            add_trace(
                data = df3[c(i, adj_points[[i]][1]), ],
                x = ~xs,
                y = ~ys,
                z = ~zs,
                mode = "lines",
                type = "scatter3d",
                # color = "yellow",
                line = list(size = 1, color = "blue")) %>%  
            add_trace(
                data = df3[c(i, adj_points[[i]][2]), ],
                x = ~xs,
                y = ~ys,
                z = ~zs,
                mode = "lines",
                type = "scatter3d",
                # color = "yellow",
                line = list(size = 1, color = "blue")) %>%  
            add_trace(
                data = df3[c(i, adj_points[[i]][3]), ],
                x = ~xs,
                y = ~ys,
                z = ~zs,
                mode = "lines",
                type = "scatter3d",
                # color = "yellow",
                line = list(size = 1, color = "blue")) %>%  
            add_trace(
                data = df3[c(i, adj_points[[i]][4]), ],
                x = ~xs,
                y = ~ys,
                z = ~zs,
                mode = "lines",
                type = "scatter3d",
                # color = "yellow",
                line = list(size = 1, color = "blue"))
        
    }
    
    fig <- fig %>%  layout(
        showlegend = F, 
        legend = list(orientation = 'h'))
    fig
    
    coolBlueHotRed <- function(n, alpha = 1) {
        rainbow(n, end=4/6, alpha=alpha)[n:1]
    }
    plot(map, type="count")
    
    df$val <- sample(x = c(0, 1), size = nrow(df), replace = T, prob = c(0.9, 0.1))
    df$val[ind] <- sample(x = c(0, 1), size = sum(ind), replace = T, prob = c(0.1, 0.9))
    
    var_unscaled <- aggregate(df$val, by=list(map$unit.classif), FUN=mean, simplify=TRUE)[ ,2]    
    plot(map, type = "property", 
         property = var_unscaled, main = "Value",
         palette.name=coolBlueHotRed)
    
