source wibble.tcl

# =============================== example code ================================

# Demonstrate Wibble if being run directly.
if {$argv0 eq [info script]} {
    # Guess the root directory.
    set root [file normalize [file dirname [info script]]]

    # Define zone handlers.
    ::wibble::handle /vars vars
    ::wibble::handle / dirslash root $root
    ::wibble::handle / indexfile root $root indexfile index.html
    ::wibble::handle / static root $root
    ::wibble::handle / template root $root
    ::wibble::handle / script root $root
    ::wibble::handle / dirlist root $root
    ::wibble::handle / notfound

    # Start a server and enter the event loop.
    catch {
        ::wibble::listen 8080
        vwait forever
    }
}
