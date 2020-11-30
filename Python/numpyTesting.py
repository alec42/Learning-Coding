import numpy as np


def create_array(nRow, nCol, Val):
    """Create numpy array of dimension `nRow` x `nCol` of the value `Val`

    Args:
        nRow (int): number of rows for the array.
        nCol (int): number of columns for the array.
        Val (float): value used to fill in the array.

    Example:
    >>> create_array(3, 2, 4.2)
    array([[4.2, 4.2],
       [4.2, 4.2],
       [4.2, 4.2]])
    """
    return np.full(nRow * nCol, Val).reshape(nRow, nCol)

def array_slicer(arrFull, m, n):
    """Extract the sub-array corresponding to the m last rows and n first columns

    Args:
        arrFull (ndarray): array of numbers to extract subarray from.
        m (int): number of rows to extract (starting from the bottom).
        n (int): number of columns to exract (starting from the left).

    Example:
    >>> array_slicer(np.random.random((20, 5)), 2, 3)
    array([[0.94888401, 0.30251725, 0.4451664 ],
       [0.52838288, 0.48531366, 0.64706118]])
    """
    return arrFull[-m:, :n]

### Fonctions statistiques et mathématiques
##  Le paquetage comprend plusieurs fonctions pour faire des statistiques.
##  +   fonction caractéristique avec `fft` (https://numpy.org/doc/stable/reference/routines.fft.html)
##  +   fonctions financières avec VP et VA (`pv()` et `fv()`), VPN (`npv()`), `pmt()`, `irr()`, etc. (https://numpy.org/doc/stable/reference/routines.financial.html)
##  +   fonctions algébriques avec `dot()`, eigenvalues, `outer()`, `inner()`, `linalg.norm()`, etc. (https://numpy.org/doc/stable/reference/routines.linalg.html)
##  +   fonctions mathématiques avec fonctions trigo (sin, cos, etc.), fonctions d'arrondissement (`round()`, `floor()`, etc.), 
##          fonctions pour sommariser les données (`prod()`, `sum()`, etc.), etc. (https://numpy.org/doc/stable/reference/routines.math.html)
##  +   fonctions pour trier, compter, etc. les données: `sort()`, `argmax()`, `where()`, etc. (https://numpy.org/doc/stable/reference/routines.sort.html)
##  +   fonctions statistiques comme stat d'ordre (`quantile()`, `amin()`, etc.), de moyenne / variance (`mean()`, `var()`, etc.),
##          fonctions d'histogramme, corrélation, etc. (https://numpy.org/doc/stable/reference/routines.statistics.html)
