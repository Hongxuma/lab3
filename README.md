# Repository for submitting labs for STAT 215A Fall 2017

This repository contains a set file structure for submitting the projects.


## Setup



1. Install Git on your system (https://git-scm.com/book/en/v2/Getting-Started-Installing-Git).

1. Sign up for GitHub (https://github.com/).

1. Go to https://education.github.com/ and sign up for the student pack to get unlimited private repositories. You are a "student" and you want an "individual account".

Once you have completed these first steps, you are then ready to create your private GitHub repository for this class.

1. Locally on your machine, clone my stat215a repository: `git clone https://github.com/rlbarter/stat215a`. This will create a copy of the repository on your own computer.

1. On the GitHub website, log in and create a **private** remote repository called *stat215a*. Add me (*rlbarter*) as a collaborator for this repository (check out settings on the repo website).

1. Back in the terminal, set the origin of your local repository to be the remote repository that you just made. Change USERNAME below to your username. This tells git which remote repository to push your changes to when you `git push` (`git remote set-url origin https://github.com/USERNAME/stat215a.git`).

1. Edit *info.txt* to reflect your own information.

```
name = "Jane Smith"
SID = "0123456789"
email = "jsmith@berkeley.edu"
github_name = "janesmith"
```

Now you're ready to push to your remote repository for the first time:

1. Check git status `git status`. You should see a bunch of text including `modified:   info.txt`.

1. Add (`git add info.txt`) and commit (`git commit -m “Updated info.txt with my own information”`) your edited *info.txt* file

1. Push your changes to your copy of the remote repository (`git push` or sometimes `git push remote origin`)

1. Check that info.txt has been updated in your remote github repository by navigating to https://github.com/USERNAME/stat215a (change USERNAME to your username)


## Submitting your lab report

You will submit your lab report by adding it to your local stat215a folder and then following the standard procedure; `git add lab1`, `git commit -m "Uploaded lab 1"`, `git push`.

Your lab should be contained in a folder called `lab1`, `lab2`, etc (depending on which lab you are submitting). The folder structure should be as follows:


```
lab1/
  extra/
  homework.pdf
  report.Rmd
  report.pdf
```

This will push your lab to your own stat215a repository (https://github.com/USERNAME/stat215a). At the submission deadline, I will then pull from your repository (since I am a collaborator), so I should receive a local copy of your lab report. Shortly afterward, I will push a folder containing two papers for you to review and grade. You will submit your feedback in the same way that you submitted your lab.
