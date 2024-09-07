# Use this file to auto-generate .out files for all checkpoint wt files for a particular 
#  network - stimulus combination - your 'testing' folder needs this .tcl file, the 
# .ex files for the stimulus set, and the priming.in file for testing. 

set netName "dynamorph_primetest"
set workingDirectory /Users/username/Desktop/recurnet_morphproc/2_testing/8t8i6o_language
set networkScript    $workingDirectory/$netName.in
set executable        lens


# Load the network
source $networkScript

# Establish proc for generating .out files from .wt files: 
proc wt2out {filename} {
  global testoutfile
  global stimSet

  # Get weight file name for generating ".out" file
  set wtfName [string trimright $filename .wt.bz2]
  append wtfName $stimSet

  # Load the weights
  loadWeights $filename
  echo loadWeights $filename

  # Open a file where you'll save the outputs
  openNetOutputFile $wtfName.out

  # test the network on the testing set 
  # (summary of test results will be appended to "testresults" file, outputs saved to filename.numupdate.out)
  puts $testoutfile $wtfName
  puts $testoutfile [test -r]

  # Close output file
  closeNetOutputFile

}

set wtFiles [glob *.wt.bz2]


set testoutfile [open "testResults_0.2prime0.0ISI4.0start.txt" "a"]
set stimSet "primetest_0.2prime0.0ISI4.0start_2021-01-18_8t6i6oexamples"
# Load the test examples
loadExamples $workingDirectory/$stimSet.ex  -exmode PERMUTE
useTestingSet $stimSet
# Call procedure wt2out for all wt files in this folder
foreach wtFile $wtFiles {
  wt2out $wtFile
}

close $testoutfile


set testoutfile [open "testResults_0.6prime0.0ISI4.0start.txt" "a"]
set stimSet "primetest_0.6prime0.0ISI4.0start_2021-01-18_8t6i6oexamples"
# Load the test examples
loadExamples $workingDirectory/$stimSet.ex  -exmode PERMUTE
useTestingSet $stimSet
# Call procedure wt2out for all wt files in this folder
foreach wtFile $wtFiles {
  wt2out $wtFile
}

close $testoutfile

set testoutfile [open "testResults_1.0prime0.0ISI4.0start.txt" "a"]
set stimSet "primetest_1.0prime0.0ISI4.0start_2021-01-18_8t6i6oexamples"
# Load the test examples
loadExamples $workingDirectory/$stimSet.ex  -exmode PERMUTE
useTestingSet $stimSet
# Call procedure wt2out for all wt files in this folder
foreach wtFile $wtFiles {
  wt2out $wtFile
}

close $testoutfile

set testoutfile [open "testResults_1.4prime0.0ISI4.0start.txt" "a"]
set stimSet "primetest_1.4prime0.0ISI4.0start_2021-01-18_8t6i6oexamples"
# Load the test examples
loadExamples $workingDirectory/$stimSet.ex  -exmode PERMUTE
useTestingSet $stimSet
# Call procedure wt2out for all wt files in this folder
foreach wtFile $wtFiles {
  wt2out $wtFile
}

close $testoutfile

set testoutfile [open "testResults_1.8prime0.0ISI4.0start.txt" "a"]
set stimSet "primetest_1.8prime0.0ISI4.0start_2021-01-18_8t6i6oexamples"
# Load the test examples
loadExamples $workingDirectory/$stimSet.ex  -exmode PERMUTE
useTestingSet $stimSet
# Call procedure wt2out for all wt files in this folder
foreach wtFile $wtFiles {
  wt2out $wtFile
}

close $testoutfile


exit