Before we begin, we need to make sure that Oasis and Rope, Lterm, and Lwt are 
installed. These can all be installed with opam with the following commands: 
- opam install oasis
- opam install rope
- opam install lambda-term
- opam install lwt

Compilation and Running CamlCode
--------------------------------
In order to compile and run this project, make sure that you are in the 
texteditor directory. Then, you can run our test cases by typing [make test].
In order to compile the code, you can type [make compile]. After you edit any 
of the project files, in order to see these changes in CamlCode, you MUST type
[make compile] before running the program. In order to run the program, you can
type [make view]. This will bring you to our welcome page. 

Here is a list of all the default commands and keyboard bindings that are
included in CamlCode: 

Keyboard Bindings
----------------
- F2: opens the in-editor command prompt.
- F3: switch between command prompt and editing area
- Ctrl + S: save current file
- Shift + Left/Right: Highlight
- Ctrl + C: copy highlighted text
- Ctrl + V: paste at cursor location
- Ctrl + Z: undo last action
- Ctrl + Y: redo last action
- Ctrl + W: close current file
- Ctrl + X: cut highlighted text 
- Ctrl + Left/Right: tab left/right, respectively.

Command Prompt Commands 
-----------------------
- "find <string>": searches file for <string>.
- "replace <string1> <string2>": searches file for <string1> and replaces the
first instance with <string2>
- "replace_all <string1> <string2>": replace all instances of <string1> in the
file with <string2>. 
- "open <file_path>": opens the file at the given file path in the text editor.
- "new <file_path>": creates a new file at the relative file path given and 
opens it in CamlCode. 
- "cd <file_path>": allows you to move within the file system, similar to UNIX
- "pwd": prints out the current working directory


