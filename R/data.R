############################### combinedPlotting.R
# Date: 5 October 2019
# Author: Bihan Zhu <bihan.zhu@mail.utoronto.ca>

###############################

#' Table6
#'
#' A dataset contain 2 factors and 6 tree nodes info
#'
#' @format A data frame with 6 rows and 4 columns:
#' \describe{
#'   \item{X}{name reference of the strains}
#'   \item{id}{id of each strain}
#'   \item{vf1}{virulence factor 1 potential of the strain with id1/id2}
#'   \item{vf2}{virulence factor 2 potential of the strain with id1/id2}
#'
#'   ...
#' }
#'
"Table6"

#' Table25
#'
#' A dataset contain 2 factors and 25 tree nodes info
#'
#' @format A data frame with 25 rows and 4 columns:
#' \describe{
#'   \item{X}{name reference of the strains}
#'   \item{id}{id of each strain}
#'   \item{vf1}{virulence factor 1 potential of the strain with id1/id2}
#'   \item{vf2}{virulence factor 2 potential of the strain with id1/id2}
#'
#'   ...
#' }
#'
"Table25"

#' Table100
#'
#' A dataset contain 2 factors and 100 tree nodes info
#'
#' @format A data frame with 100 rows and 4 columns:
#' \describe{
#'   \item{X}{name reference of the strains}
#'   \item{id}{id of each strain}
#'   \item{vf1}{virulence factor 1 potential of the strain with id1/id2}
#'   \item{vf2}{virulence factor 2 potential of the strain with id1/id2}
#'
#'   ...
#' }
#'
"Table100"

#' Table150
#'
#' A dataset contain 2 factors and 150 tree nodes info
#'
#' @format A data frame with 150 rows and 4 columns:
#' \describe{
#'   \item{X}{name reference of the strains}
#'   \item{id}{id of each strain}
#'   \item{vf1}{virulence factor 1 potential of the strain with id1/id2}
#'   \item{vf2}{virulence factor 2 potential of the strain with id1/id2}
#'
#'   ...
#' }
#'
"Table150"


#' Tree25
#'
#' A "phylo" object contain information about the tree
#' created from the sample25.newick file contains taxonomy info of all strains
#'
#' @format A phylo object with 4 main parts
#' \describe{
#'   \item{edge}{The matrix edge contains the beginning and ending node number for all the nodes and tips in the tree. By convention, the tips of the tree are numbered 1 through n for n tips; and the nodes are numbered n + 1 through n + m for m nodes. m = n - 1 for a fully bifurcating tree. This is just to keep track of which nodes are internal and which are leaves.}
#'   \item{tip.label}{The vector tip.label contains the labels for all the tips in the tree. The order of tip.label is the order of the tips numbered 1 through n in edge.}
#'   \item{edge.length}{a vector of class "numeric" containing all the edge lengths of the tree in the same order as the rows in edge; and root.edge, a numeric value giving the length of the root edge, if one exists. }
#'   \item{Nnode}{The integer Nnode contains the number of internal nodes in the tree, including the root of the tree if the tree is rooted.}
#'
#'   ...
#' }
#'
#' @source http://www.phytools.org/eqg/Exercise_3.2/
#'
"Tree25"

#' Tree100
#'
#' A "phylo" object contain information about the tree
#' created from the sample100.newick file contains taxonomy info of all strains
#'
#' @format A phylo object with 4 main parts
#' \describe{
#'   \item{edge}{The matrix edge contains the beginning and ending node number for all the nodes and tips in the tree. By convention, the tips of the tree are numbered 1 through n for n tips; and the nodes are numbered n + 1 through n + m for m nodes. m = n - 1 for a fully bifurcating tree. This is just to keep track of which nodes are internal and which are leaves.}
#'   \item{tip.label}{The vector tip.label contains the labels for all the tips in the tree. The order of tip.label is the order of the tips numbered 1 through n in edge.}
#'   \item{edge.length}{a vector of class "numeric" containing all the edge lengths of the tree in the same order as the rows in edge; and root.edge, a numeric value giving the length of the root edge, if one exists. }
#'   \item{Nnode}{The integer Nnode contains the number of internal nodes in the tree, including the root of the tree if the tree is rooted.}
#'
#'   ...
#' }
#'
#' @source http://www.phytools.org/eqg/Exercise_3.2/
#'
"Tree100"

#' Tree150
#'
#' A "phylo" object contain information about the tree
#' created from the sample150.newick file contains taxonomy info of all strains
#'
#' @format A phylo object with 4 main parts
#' \describe{
#'   \item{edge}{The matrix edge contains the beginning and ending node number for all the nodes and tips in the tree. By convention, the tips of the tree are numbered 1 through n for n tips; and the nodes are numbered n + 1 through n + m for m nodes. m = n - 1 for a fully bifurcating tree. This is just to keep track of which nodes are internal and which are leaves.}
#'   \item{tip.label}{The vector tip.label contains the labels for all the tips in the tree. The order of tip.label is the order of the tips numbered 1 through n in edge.}
#'   \item{edge.length}{a vector of class "numeric" containing all the edge lengths of the tree in the same order as the rows in edge; and root.edge, a numeric value giving the length of the root edge, if one exists. }
#'   \item{Nnode}{The integer Nnode contains the number of internal nodes in the tree, including the root of the tree if the tree is rooted.}
#'
#'   ...
#' }
#'
#' @source http://www.phytools.org/eqg/Exercise_3.2/
#'
"Tree150"

