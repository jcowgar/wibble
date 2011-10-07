#!/usr/bin/tclsh8.6
# change the above line to point to the tclsh8.6 executable

lappend auto_path .
package require wibble

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

    # Start a server and enter the event loop if not already there.
    catch {
        ::wibble::listen 8080
        if {!$tcl_interactive} {
			vwait forever
		}
    }
}
