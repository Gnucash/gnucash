

SET(pycons_DATA
        __init__.py
        console.py
        ishell.py
        pycons
        setup.py
        shell.py
        simple_plot.py
)

SET_DIST_LIST(pycons_DIST CMakeLists.txt ${pycons_DATA})