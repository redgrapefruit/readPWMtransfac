# R function for reading standard transfac PWM file. (Not transfac-like format)

readPWMtransfac <- function(file)
{
        table <- read.table(file = file, fill=T)
        table.vector <- cbind(as.vector(table[[1]]), as.vector(table[[2]]), 
                              as.vector(table[[3]]), as.vector(table[[4]]), as.vector(table[[5]]))
        ID <- which(table.vector == "ID")
        P0 <- which(table.vector == "P0")
        BA <- which(table.vector == "BA")
        names <- table.vector[ID, 2]
        listPWM <- list()
        for (i in seq(length(ID))) {
                listPWM[[i]] <- matrix(as.numeric(table.vector[(P0[i] + 1):(BA[i] - 2), 2:5]), 
                												nrow = 4, byrow = T, dimnames = list(c("A", "C", "G", "T")))
        colnames(listPWM[[i]]) <- 1:(length(listPWM[[i]])/4)
        names(listPWM)[i] <- table.vector[ID[i], 2]
        }
        return(listPWM)
}
