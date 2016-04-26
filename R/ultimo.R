ultimo = function(x, na.rm = TRUE) {
        if (all(is.na(x))) {
            return(NA)
        }
        else {
            aux = which(is.na(x) == FALSE)
            return(x[aux[length(aux)]])
        }
        }
