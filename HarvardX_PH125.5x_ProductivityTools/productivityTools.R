# Productivity Tools

# Installing R and RStudio
# Which of the following statements about R and RStudio is true?
# R is a programming language, whereas RStudio is a desktop environment.    <-*
# You can use RStudio without using R, but we recommend using R in this course.
# When you download RStudio, it automatically downloads and installs R too.
# You can only use R on Mac OS X and Linux. Windows users must use RStudio.




# Comprehension Check: Introduction to RStudio
# Introduction to RStudio
# Select the code that will NOT install the popular graphing and data manipulation packages ggplot2 and dplyr in R.
# install.packages(c("ggplot2","dplyr"))
# install.packages("tidyverse")
install.packages(c('dplyr', 'ggplot2')) # <-

# install.packages("ggplot2")
# install.packages("dplyr")



# Comprehension Check: Introduction to Git and GitHub
# Pull
# What does the term "pull" mean in the context of using Git in RStudio?
# Add local files to a remote GitHub repo.
# Download changes from the remote repo to your local repository.    <-*
# Configure the RStudio environment to automatically connect to GitHub.
# Save changes made in RStudio to the local repository on your computer.



# Push
# What does the term “push” mean in the context of using Git in RStudio?
# Upload changes made in your local repository to a remote repository.   <-*
# Download changes from the remote repo to the RStudio environment.
# Configure the RStudio environment to automatically connect to GitHub.
# Save changes made in RStudio to the local repository on your computer.



# Commit
# What does the term “commit” mean in the context of using Git in RStudio?
# Add local files to a remote GitHub repo.
# Download changes from the remote repo to the RStudio environment.
# Configure the RStudio environment to automatically connect to GitHub.
# Save changes made in RStudio to the local repository on your computer.    <-*



# Comprehension Check: Introduction to Unix
# Working Directory
# It is important to know which directory, or folder, you’re in when you are working from the command line in Unix. Which line of code will tell you the current working directory?
# cd
# pwd    <- *
# rm
# echo




# Previously Executed Code
# You can’t use your computer’s mouse in a terminal. How can you see a line of code that you executed previously?
# Type pwd
# Type echo
# Use the up arrow    <-*
# Press the enter key



# pwd Output
# Assume a student types pwd and gets the following output printed to the screen: /Users/student/Documents.
#
# Then, the student enters the following commands in sequence:
#
# mkdir projects
#
# cd projects
#
# What will be printed to the screen if the student types pwd after executing the two lines of code shown above?
# /Users/student/Documents
# /Users/student/Documents/projects   <-*
# /Users/student
# cd: projects: No such file or directory




# Moving Files 1
# The following is the full path to a your homework assignment file called "assignment.txt": /Users/student/Documents/projects/homeworks/assignment.txt.
#
# Which line of code will allow you to move the assignment.txt file from the “homeworks” directory into the parent directory “projects”?
# mv assignment.txt
# mv assignment.txt .
# mv assignment.txt ..   <-*
# mv assignment.txt /projects



# Moving Files 2
# You want to move a file called assignment.txt file into your projects directory. However, there is already a file called "assignment.txt" in the projects directory.
#
# What happens when you execute the “move” (mv) command to move the file into the new directory?
# The moved "assignment.txt" file replaces the old "assignment.txt" file that was in the "projects" directory with no warning.    <-*
# An error message warns you that you are about to overwrite an existing file and asks if you want to proceed.
# An error message tells you that a file already exists with that name and asks you to rename the new file.
# The moved “assignment.txt” file is automatically renamed “assignment.txt (copy)” after it is moved into the “projects” directory.




# Unix Commands
# Which of the following statements does NOT correctly describe the utility of a command in Unix?
# The  key exits the viewer when you use less to view a file.
# The command ls lists files in the current directory.
# The command mkdir makes a new directory and moves into it. s   <- *
# The mv command can move a file and change the name of a file.



# Why R Markdown?
# Why might you want to create a report using R Markdown?
# R Markdown has better spell-checking tools than other word processors.
# R Markdown allows you to automatically add figures to the final document.   <-*
# R Markdown final reports have smaller file sizes than Word documents.
# R Markdown documents look identical to the final report.





# Naming an R Chunk
# You have a vector of student heights called heights. You want to generate a histogram of these heights in a final report, but you don’t want the code to show up in the final report. You want to name the R chunk “histogram” so that you can easily find the chunk later.
#
# Which of the following R chunks does everything you want it to do?
#
# ```{r, histogram, echo=FALSE}
# hist(heights)
# ```

# ```{r histogram}
# hist(heights)
# ```

# ```{r, echo=FALSE}
# hist(heights)
# ```

# ```{r histogram, echo=FALSE}    <-*
# hist(heights)
# ```




# R Markdown Report
# Below is a section of R Markdown code that generates a report.
#
# ---
#     title: "Final Grade Distribution"
# output: pdf_document
# ---
#     ```{r, echo=FALSE}
# load(file="my_data.Rmd")
# summary(grades)
# ```
# Select the statement that describes the file report generated by the R markdown code above.
# A PDF document called “Final Grade Distribution” that prints a summary of the “grades” object. The code to load the file and produce the summary will not be included in the final report.    <-*
# A PDF document called “Final Grade Distribution” that prints a summary of the “grades” object. The code to load the file and produce the summary will be included in the final report.
# An HTML document called “Final Grade Distribution” that prints a summary of the “grades” object. The code to load the file and produce the summary will not be included in the final report.
# A PDF document called “Final Grade Distribution” that is empty because the argument echo=FALSE was used.




# Report File Format
# 1 point possible (graded)
# The user specifies the output file format of the final report when using R Markdown.
#
# Which of the following file types is NOT an option for the final output?
# .rmd    <-*
# .pdf
# .doc
# .html





# Git and GitHub Benefits
# Which statement describes reasons why we recommend using git and Github when working on data analysis projects?
# Git and Github facilitate fast, high-throughput analysis of large data sets.
# Git and Github allow easy version control, collaboration, and resource sharing.    <-*
# Git and Github have graphical interfaces that make it easy to learn to code in R.
# Git and Github is good for long-term storage of private data.




# Cloning a Repo
# Select the steps necessary to:
#
# 1. create a directory called “project-clone”,
# 2. clone the contents of a git repo at the following URL into that directory (https://github.com/user123/repo123.git), and
# 3. list the contents of the cloned repo.
#
# mkdir project-clone
# git add https://github.com/user123/repo123.git
# ls
#
# mkdir project-clone
# git clone https://github.com/user123/repo123.git
# ls
#
# mkdir project-clone      <-*
# cd project-clone
# git clone https://github.com/user123/repo123.git
# ls
#
# mkdir project-clone
# cd project-clone
# git clone https://github.com/user123/repo123.git
# less





# Git Status
# You have successfully cloned a Github repository onto your local system. The cloned repository contains a file called “heights.txt” that lists the heights of students in a class. One student was missing from the dataset, so you add that student’s height using the following command:
#
# echo “165” >> heights.txt
# Next you enter the command git status to check the status of the Github repository.
#
# What message is returned and what does it mean?
#     modified: heights.txt, no changes added to commit
# This message means that the heights.txt file was modified, but the changes have not been staged or committed to the local repository.    <-*
# modified: heights.txt, no changes added to commit
# This message means that the heights.txt file was modified and staged, but not yet committed.
# 1 file changed
# This message means that the heights.txt file was modified, staged, committed, and pushed to the upstream repository.
# modified: heights.txt
# This message means that the heights.txt file was modified, staged, and committed.
# unanswered
# Submit You have used 0 of 2 attempts Some problems have options such as save, reset, hints, or show answer.





# Modifying a File in an Upstream Repo
# You cloned your own repository and modified a file within it on your local system. Next, you executed the following series of commands to include the modified file in the upstream repository, but it didn’t work. Here is the code you typed:
#
# git add modified_file.txt
# git commit -m “minor changes to file” modified_file.txt
# git pull
# What is preventing the modified file from being added to the upstream repository?
#     The wrong option is being used to add a descriptive message to the commit.
# git push should be used instead of git pull.    <-*
# git commit should come before git add.
# The git pull command line needs to include the file name.




# Comprehension Check: Creating a GitHub Repository
# Bookmark this page
# Readme File
# You have a directory of scripts and data files on your computer that you want to share with collaborators using GitHub. You create a new repository on your GitHub account called “repo123” that has the following URL: https://github.com/user123/repo123.git.
#
# Which of the following sequences of commands will convert the directory on your computer to a Github directory and create and add a descriptive “read me” file to the new repository?

# git init
# git add README.txt
# git commit -m "First commit. Adding README file."
# git remote add origin `https://github.com/user123/repo123.git`
# git push

# echo “A new repository with my scripts and data” > README.txt
# git init
# git add
# git commit -m "First commit. Adding README file."
# git remote add origin `https://github.com/user123/repo123.git`
# git push

# echo “A new repository with my scripts and data” > README.txt
# git init
# git add README.txt
# git commit -m "First commit. Adding README file."
# git remote add origin `https://github.com/user123/repo123.git`
# git pull

# echo “A new repository with my scripts and data” > README.txt   <-*
# git init
# git add README.txt
# git commit -m "First commit. Adding README file."
# git remote add origin `https://github.com/user123/repo123.git`
# git push





# Arguments
# What will the command ls -lat produce?
# A list of all file (names, sizes, and other information) arranged in chronological order with the most recently modified files at the top of the list.   <-*
# A list of visible files (names, sizes, and other information) arranged in chronological order with the oldest files at the top of the list.
# A list of all files (names only) arranged in chronological order with the oldest files at the top of the list.
# A list of visible files (names only) arranged in chronological order with the most recent files at the top of the list.





# Arguments 2
# What happens when you remove a directory using the command rm -r?
# You cannot remove a directory using the rm command.
# You permanently remove the entire directory, including all files and subdirectories.   <-*
# You move the entire directory to a trash folder, but it can be restored later.
# You get a warning message asking if you want to proceed, then you delete the directory.




# Getting Help and Pipes
# By default, the head command in Unix displays the first 10 lines of a specified file. You can change the number of lines using an argument that indicates the numeric value of the desired number of lines.

# Which of the following commands displays only the first 6 lines of a manual for the ls command?
# man ls -6 | head
# head | man ls -6
# head -6 | man ls
# man ls | head -6   <-*




# Wildcards
# You have a directory containing the following files.
#
# data1.csv, data2.txt, data3.txt, Data8.csv, data13.csv, data18.txt, Data22.txt, Data34.csv
#
# Which command will list only all of the .txt files that have “data” in their name? Remember that commands are case-sensitive.
# ls data*
# ls data*   <-*
# ls *.txt
# ls data?.txt





# Wildcards 2
# You have a directory containing the following files.
#
# data1.csv, data2.txt, data3.txt, Data8.csv, data13.csv, data18.txt, Data22.txt, Data34.csv
#
# Which command will remove every file that begins with “D”?
# rm D    <-*
# rm D*.txt
# ls D*
# ls D*.txt




# Wildcards 3
# Imagine you have multiple text files in the following directory: /Users/student/Documents/project.
#
# You enter the following commands in sequence:
#
# mkdir data
# mv *.txt data
# cd data
# What will be printed to the screen if you enter the ls command after executing the three lines of code shown above?
# /Users/student/Documents/project/data
# The file names that were moved from the “project” directory into the “data” directory.   <-*
# Nothing. You haven’t added anything to the new “data” directory yet.
# The file names that remain in the “project” directory.




# Environment Variables and Shells
# What does the command echo $HOME do?
# Moves into to the home directory.
# Makes the current directory the home directory.
# Prints the path to the home directory.   <-*
# Prints “$HOME” to the screen.





# Environment Variables and Shells 2
# Many systems operate using the Unix shell and command language, bash. Each time you start using bash, it executes the commands contained in a “dot” file. Your “dot” file may be called something like “.bash_profile” or “.bash_rc”.
#
# Which command will let you see your “dot” files?
# ls -a   <-*
# ls bash*
# head *bash*
# ls -l



# Executables, Permissions, and File Types
# Your colleague was editing his “dot” files when something went wrong. He first noticed there was an issue when he tried to execute the following line of code:
# ls
# He received the following error:
#
# -bash: ls: command not found
# What could have happened to cause this error?
# He is trying to execute ls which is a bash command, but his system isn’t running bash as a shell.
# The command ls doesn’t exist. He should be using the command ll.
# He forgot to specify a file name to be listed. The command ls * should work.
# He changed the information contained in $PATH. Now the system cannot find the executable file for ls.   <-*




# Executables, Permissions, and File Types 2
# The bash profile in your home directory contains information that the bash shell runs each time you use it. You can customize the information in your bash profile to tell your system to do different things. For example, you can make an “alias”, which acts like a keyboard shortcut.
#
# Which line of code, when added to your bash profile, will let you print “seetop” to view the name, size, and file type of the 10 most recently added visible files?
# alias seetop=’ls -lt’
# alias seetop=’ls -lt | head’   <-*
# alias seetop=’ls -t | head’
# alias seetop=’head | ls -l’
