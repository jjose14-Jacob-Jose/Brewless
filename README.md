# SER502-Group2
SER 502 SP2023 Group project

# Build
Built on ~~Windows~~ Ubuntu.

# Tools
Using python, lex, and DCG

# Installation
Following must be installed in Ubuntu to call Prolog from Python.

1.   sudo apt install python3.10-venv
2.   python3 -m venv pyswip_env 
3.   sudo apt install swi-prolog
4.   sudo apt install python3-pip
5.   pip install pyswip

# Running
TBD

# Scripts
Build compiler: TBD
Runs at runtime: TBD

# Youtube
TBD


----------------------------------------------------
### brewlessInterpreter.py (running)
Format:
python3 brewlessInterpreter.py <path_to_prolog_file.pl> <prolog_query> <output_variable_name>

Example
python3 brewlessInterpreter.py assignSemantics.pl "execute_program([begin, var, z, ; , var, x, ;, z, :=, x, end, .], Z)." "Z"

