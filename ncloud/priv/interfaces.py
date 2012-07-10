import pluggdapps.core import Interface, Attribute

class IProcessPool( Interface ):
    """Do concurrent programming using a process pool."""

    def __init__( *args ):
        """Initialize a process pool."""

    def pmap( func, datalist ):
        """Apply function `func` concurrently on each element in list
        `datalist`."""
