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
#'   \item{X.1}{name reference of the strains}
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


#' VF25_2
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
"VF25_2"

#' VF25_3
#'
#' A dataset contain 3 factors and 25 tree nodes info
#'
#' @format A data frame with 25 rows and 4 columns:
#' \describe{
#'   \item{X}{name reference of the strains}
#'   \item{id}{id of each strain}
#'   \item{vf1}{virulence factor 1 potential of the strain with id1/id2}
#'   \item{vf2}{virulence factor 2 potential of the strain with id1/id2}
#'   \item{vf3}{virulence factor 3 potential of the strain with id1/id2}
#'
#'   ...
#' }
#'
"VF25_3"

#' VF25_4
#'
#' A dataset contain 4 factors and 25 tree nodes info
#'
#' @format A data frame with 25 rows and 4 columns:
#' \describe{
#'   \item{X}{name reference of the strains}
#'   \item{id}{id of each strain}
#'   \item{vf1}{virulence factor 1 potential of the strain with id1/id2}
#'   \item{vf2}{virulence factor 2 potential of the strain with id1/id2}
#'   \item{vf3}{virulence factor 3 potential of the strain with id1/id2}
#'   \item{vf4}{virulence factor 4 potential of the strain with id1/id2}
#'
#'   ...
#' }
#'
"VF25_4"


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

#' VF100
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
"VF100"

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

#' VF150
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
"VF150"


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

#' tree_df_25
#'
#' A data frame contain information about the tree
#' created from the sample150.newick file contains taxonomy info of all strains
#'
#' @format data frame with 9 columns and 49 rows
#' \describe{
#'   \item{depth}{depth of the node}
#'   \item{id}{ id of this node}
#'   \item{parent}{ the parent of this point / NA if root}
#'   \item{angle}{ angle of this point}
#'   \item{c1}{ child 1 id / -1 if no child}
#'   \item{c2}{ child 2 id / -1 if no child}
#'   \item{c1_a}{ child 1 angle }
#'   \item{c2_a}{ child 2 angle}
#'   \item{depth_length}{ this is the parameter to calculate depth between parent and self if internal node/ length to outer radius if tips }
#'
#'   ...
#' }
#'
#'
"tree_df_25"

#' xy_df_25
#'
#' A data frame contain information about the tree
#' created from the sample150.newick file contains taxonomy info of all strains
#'
#' @format data frame with 11 columns and 49 rows
#' \describe{
#'   \item{depth}{depth of the node}
#'   \item{id}{ id of this node}
#'   \item{parent}{ the parent of this point / NA if root}
#'   \item{angle}{ angle of this point}
#'   \item{c1}{ child 1 id / -1 if no child}
#'   \item{c2}{ child 2 id / -1 if no child}
#'   \item{c1_a}{ child 1 angle }
#'   \item{c2_a}{ child 2 angle}
#'   \item{depth_length}{ this is the parameter to calculate depth between parent and self if internal node/ length to outer radius if tips }
#'   \item{x}{ x coordinate of the point}
#'   \item{y}{ y coordinate of the point}
#'
#'   ...
#' }
#'
#'
"xy_df_25"


#' tree_layers_25
#'
#' A data frame contain information about the tree
#' created from the sample150.newick file contains taxonomy info of all strains
#'
#' @format data frame with 3 columns and 7033 rows
#' \describe{
#'   \item{x}{x coordinate of the point}
#'   \item{y}{y coordinate of the point}
#'   \item{layer_id}{ id to show which radius the point is on}
#'
#'   ...
#' }
#'
#'
"tree_layers_25"


