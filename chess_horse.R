#!/usr/bin/R

#The horse jumping randomly and shit code


#TODO
#convert positions back into a 8x8 matrix
#and plot it...

possible_horse_positions = function(pos){        #pos being a vector (x,y)
    possible_jumps = rbind(c(pos[1]+1,pos[2]+2), c(pos[1]+1,pos[2]-2), c(pos[1]-1,pos[2]+2), c(pos[1]-1,pos[2]-2), c(pos[1]+2,pos[2]+1), c(pos[1]+2,pos[2]-1), c(pos[1]-2,pos[2]+1), c(pos[1]-2,pos[2]-1))

    #print(possible_jumps)      #all jumps without filtering
    
    return(possible_jumps[possible_jumps[,1]>0 & possible_jumps[,1]<9 & possible_jumps[,2]>0 & possible_jumps[,2]<9,])  #removing those out of the chessboard

}

coordinates_into_rowposition = function(coord){
    # a function that converts coordinates into position in a row of length 64 
    # So since we multiply the transition matrix M, n times than by the starting position X, our position at time t is described by:
    # X(t) = M^n * X(0)   //  which means X(0) is a column-vector and the probabilities of the positions in the matrix M are described "by row" (each row represents a starting position)
    # So let's choose a direction of conversion :
    # ...
    # D
    # C
    # B 9 ...
    # A 1 2 3 4 5 6 7 8
    #   A B C D E F G H
    #
    return(coord[1]+8*(coord[2]-1))
}

multiple_coordinates_into_row_position = function(coord_matrix){
    #takes a matrix of coordinates, returns the positions in a row of 64
    all_positions = apply(coord_matrix, 1, coordinates_into_rowposition)
    return(all_positions)
}

row_probabilities = function(row_positions){
    #takes the results from multiple_coordinates_into_row_position (multiple indexes of the positions) and
    #converts it into a row a probabilities of length 64, with 0 where the horse can't go and P>0 the probability of the other positions
    r = rep(0,64)
    r[row_positions] = 1/length(row_positions)
    return(r)
}

create_trans_matrix = function(){   #we expect 8
    #creates the transition matrix. basically just call all the previous functions, in order and in order of chessboard positions (left to right, bottom to top)
    m = c()
    for(i in 1:8){   
        for(j in 1:8){
            m = rbind(m,row_probabilities(multiple_coordinates_into_row_position(possible_horse_positions(c(j,i)))))
        }
    }
    return(m)
}

Nth_transition_matrix = function(trans_matrix,N){
    #returns the product of multiplying trans_matrix N times
    m1 = trans_matrix
    if(N > 1){
        for(i in 1:(N-1)){
            m1 = m1 %*% trans_matrix
        }
    }
    return(m1)
}

get_initial_position = function(pos){ 
    # from the initial index, returns the row of 0s and one 1 at initial position
    vec = rep(0,64)
    vec[pos] = 1
    return(vec)
}       

final_frequencies = function(N,initial_pos){
    # returns M^N * M0 where M is the matrix of transition, N the timepoint and M0 the initial position matrix
    return(t(Nth_transition_matrix(create_trans_matrix(), N)) %*% get_initial_position(initial_pos))   #kind of a mystery why I have to transpose the trans_matrix. I guess it's just dimensions
}

row_of_64_into_matrix = function(vec){
    #takes a vector of 64 and returns its matrix form 8x8
    m = matrix(vec,byrow = T, nrow = 8)
    return(m[nrow(m):1,])                       #we have to reverse the matrix vertically
}

main = function(){

    start_pos = 1       #CHANGE STARTING POSITION HERE
    N = 20              #CHANGE NUMBER OF TURNS HERE

    xlabels = c('A','B','C','D','E','F','G','H')

    dir.create("frames", showWarnings = FALSE)
    setwd("frames")

    png(file="frame%02d.png", width=480, height=480)

    for (i in 1:N){
        m = row_of_64_into_matrix(final_frequencies(i,start_pos))

        heatmap(t(m[nrow(m):1,]), Colv = NA, Rowv = NA, labCol = xlabels,symm=T)    #stupid heatmap reverts the matrix again ?!
        title(sub=paste("turn",i,sep=" "), adj=1, line=3, font=2)
    }

    #convert those png into a gif
    #this only works on linux systems (requires imagemagick)
    system("convert -delay 60 *.png animated_horse.gif")

    #and remove the pngs
    file.remove(list.files(pattern=".png"))
}

main()
