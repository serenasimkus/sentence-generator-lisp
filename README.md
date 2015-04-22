# sentence-generator-lisp

This was an assignment for the artificial intelligence course at Columbia University. The assignment was to take skeleton Lisp code, and modify it by improving the grammar and the quality of the sentences created.

To run the assignment code using clisp (you may need to download clisp):

  > $ clisp
  
In the CLISP interpreter run:

  > // to load the file into clisp
  > (load "grammar_sks2187.lisp")
  
  > // to generate the first targeted sentence
  > (targeted-sentence rules1)
  > // can also generate sentences 2-5 by changing rulesx to desired sentence number
  
  > // to run the loop and generate 10,000 invalid and valid sentences into one file
  > (loop-run 10000)
  
At this point you can use grep to separate the valid and invalid sentences. 

  > $ cat output_sks2187.txt | grep '+ ' > valid.txt
  > $ cat output_sks2187.txt | grep '\- ' > invalid.txt
