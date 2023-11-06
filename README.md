## Karazin Scala Users Group
# Scala Course 2023-2024 Autumn

## What to do
* Create GitHub account (if needed)
* Clone (not fork) the repo
* Create your own private repo
* Push the local clone to your private repo
* Create a new branch for each homework


## Create GitHub account
Open https://github.com/ and follow the sign-up procedure. 
It's better to choose a professional-like name, usually `[First][Last]` name 
(my personal GitHub account is https://github.com/IgorWolkov).

## Clone the repo
Course [repo](https://github.com/KarazinScalaUsersGroup/scala-course-2023-2024-autumn).

Please **do not** fork the repo.

Clone the repo to the local machine.
```shell
git clone https://github.com/KarazinScalaUsersGroup/scala-course-2023-2024-autumn.git
```
To verify that all works, go to the folder with the source code and run 
```shell
sbt clean compile
```

## Remove .git
In the clonned repo folder there is a hidden folder `.git`. Remove it.

## Initialize new repo
For initializing a new repo run in the folder with the code
```shell
git init
```
then add all local files 
```shell
git add .
```
then commit the files
```shell
git commit -m "Initializing the repo"
```
a commit message after `-m` flag could be customized.

## Create a new repository on GitHub
Create a new repository on GitHub. Please name it as the original repo `scala-course-2023-2024-autumn`.

## Open the newly created repo
Open your new repo and follow the instructions in `…or push an existing repository from the command line` section.

After pushing the code to your local repository you will have "original" code in `main` branch.

## New branch for each homework 
**Do not** solve the homework tasks in `main` branch. You required to create a new branch for each homework.
Create a branch for a new homework only when the previous homework is accepted and merged to `main` branch.
Naming convention for homework branches: 
* `week-1` for week 1
* `week-2` for week 2
* ...
* `week-n` for week n
Please follow this name convention.

