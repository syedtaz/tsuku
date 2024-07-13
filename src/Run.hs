module Run where

data Command
  = -- Idea borrowed from opam. A switch is essentially a separate installation
    -- of a compiler alongside a set of packages. You can have multiple versions
    -- of the same compiler with different package constraints.
    Switch

data Switch
  = -- Show the name and compiler version of the current switch.
    Which
  | -- Create a new switch with a specified compiler version or commit hash.
    Create
  | -- Changes the current switch.
    Set
  | -- List currently installed switches.
    List
  | -- List available compiler versions.
    ListAvailable
