module Arch.Machine where

data Machine = Machine {
    halted :: Bool    
}

halt :: Machine -> Machine
halt m = m { halted = True }

initial :: Machine
initial = Machine {
        halted = False
    }