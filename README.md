# roomutils

The "roomutils" package includes functions to retrieve information about the users in the chatrooms at Stack Overflow.

## Installation

The package is in the "room_utils" subdirectory. Install it using "devtools".

```r
devtools::install_github("r-public/room_utils", subdir = "room_utils")
```

## Functions

Function | Description
----------|------------
`room_owners` | Lists the Room Owners of a Specified Stack Overflow Room
`room_rep` | Sum of the Reputation of Current Room Occupants
`user_list` | Lists the Locations of Users in a Specified Room
`user_location` | Lists the Locations of Specified Users
`plot_current_users` | Plots the Locations of Users in a Specified Room
