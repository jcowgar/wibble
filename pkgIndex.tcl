# package index for wibble
if {![package vsatisfies [package provide Tcl] 8.6]} {error "Tcl 8.6 is required"}
package ifneeded wibble 0.1 [list source [file join $dir wibble.tcl]]
