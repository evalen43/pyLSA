import numpy as np
from scipy.sparse import coo_matrix
from scipy.sparse.linalg import inv
from scipy.sparse import csc_matrix
import networkx as nx
from scipy.sparse import csr_matrix

class matrix:
    @staticmethod
    def optimize_bandwidth(node_coords, member_incidences):
        """ \brief This function optimizes the bandwidth of a symmetric matrix using nodes 
        coordinates and element incidences as edges. It returns the optimized matrix and 
        the permutation vector.
        \note This code takes the node coordinates and member incidences as input and creates 
        an adjacency matrix representing the edges between nodes. It then converts 
        the adjacency matrix to a sparse matrix and computes the bandwidth of the matrix. 
        The matrix is then permuted to optimize the bandwidth, and the new bandwidth and 
        permutation vector are computed. \return The optimized matrix and permutation vector 
        are returned as output."""
        num_nodes = len(node_coords)
        num_members = len(member_incidences)
        # Create the adjacency matrix
        adj_matrix = np.zeros((num_nodes, num_nodes))
        for member in member_incidences:
            i, j = member
            adj_matrix[i, j] = 1
            adj_matrix[j, i] = 1
        # Convert the adjacency matrix to a sparse matrix
        sparse_matrix = coo_matrix(adj_matrix)
        # Compute the bandwidth of the matrix
        bandwidth = sparse_matrix.bandwidth
        # Permute the matrix to optimize the bandwidth
        permuted_matrix = sparse_matrix.permute(np.argsort(node_coords[:, 0]))
        # Compute the new bandwidth of the matrix
        new_bandwidth = permuted_matrix.bandwidth
        # Compute the permutation vector
        permutation = np.argsort(node_coords[:, 0])
        return (permuted_matrix, permutation)

    @staticmethod
    def grid(num_rows,num_cols,spacing):
        # Create node coordinates
        x_coords = np.arange(num_cols) * spacing
        y_coords = np.arange(num_rows) * spacing
        node_coords = np.transpose([np.tile(x_coords, num_rows), np.repeat(y_coords, num_cols)])

        # Create member incidences
        member_incidences = []
        for i in range(num_rows):
            for j in range(num_cols):
                node_num = i * num_cols + j
                if j < num_cols - 1:
                    member_incidences.append([node_num, node_num + 1])
                if i < num_rows - 1:
                    member_incidences.append([node_num, node_num + num_cols])
        # Print node coordinates and member incidences
        print("Node Coordinates:")
        print(node_coords)
        print("Member Incidences:")
        print(member_incidences)
        
    @staticmethod
    def sparse_matrix(data,row,col):
        # Create a sparse matrix
        """ data = [1, 2, 3, 4, 5, 6]
        row = [0, 1, 2, 0, 1, 2]
        col = [0, 1, 2, 3, 4, 5] """
        sparse_matrix = csc_matrix((data, (row, col)), shape=(3, 6))

        # Compute the inverse of the sparse matrix
        inv_sparse_matrix = inv(sparse_matrix)
        return inv_sparse_matrix


def optimize_band(nodes, edges):
    """
    @brief Optimize the bandwidth of a symmetric matrix using nodes and edges.

    @param nodes List of node coordinates.
    @param edges List of member incidences as edges.

    @return The optimized symmetric matrix as a numpy.ndarray.
    """

    # Create a graph from the nodes and edges
    G = nx.Graph()
    G.add_nodes_from(nodes)
    G.add_edges_from(edges)

    # Get the reverse Cuthill-Mckee ordering of the nodes
    rcm = list(nx.utils.reverse_cuthill_mckee_ordering(G))

    # Create a mapping from old nodes to new nodes
    mapping = {old_node: new_node for new_node, old_node in enumerate(rcm)}

    # Relabel the nodes in the graph according to the new ordering
    G = nx.relabel_nodes(G, mapping)

    # Create a sparse matrix from the graph
    A = nx.to_scipy_sparse_matrix(G, nodelist=range(len(G)), format='csr')

    # Convert the sparse matrix to a dense matrix
    matrix = A.todense()

    return matrix   


def optimize_bandwidth(node_coords, member_incidences):
    """
    @brief This function optimizes the bandwidth of a symmetric matrix using nodes 
    coordinates and element incidences as edges. It returns the new relabeled nodes and 
    relabeled member incidences.

    @param node_coords The coordinates of the nodes.
    @param member_incidences The incidences of the members.

    @return A tuple containing the new relabeled nodes and relabeled member incidences.
    """

    # Create a graph from the member incidences
    G = nx.Graph()
    G.add_edges_from(member_incidences)

    # Compute the Reverse Cuthill-Mckee ordering
    rcm = list(nx.utils.reverse_cuthill_mckee_ordering(G))

    # Create a mapping from old nodes to new nodes
    mapping = {old: new for new, old in enumerate(rcm)}

    # Relabel the nodes in the graph according to the new ordering
    G = nx.relabel_nodes(G, mapping)

    # Relabel the node coordinates
    new_node_coords = node_coords[rcm]

    # Relabel the member incidences
    new_member_incidences = [(mapping[i], mapping[j]) for i, j in member_incidences]

    return new_node_coords, new_member_incidences 

        
