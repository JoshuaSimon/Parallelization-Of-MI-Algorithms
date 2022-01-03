# Install R using Windows Subsystem for Linux (WSL)

To follow this guide you need the latest version of Windows 10 (check for updates) and a CPU that is supporting virualization.

## 1 Enable virtualization on your CPU

This step is specific to your CPU/mainboard or laptop manufacturer. In generel, you have to enter your UEFI BIOS by booting directly into that and not into Windows. Virualization if often found under advanced settings in the UEFI BIOS at the CPU tab.

## 2 Install WSL and a Linux distribution

Follow the steps in this video: https://www.youtube.com/watch?v=n-J9438Mv-s&t=278s or go to Windows Serach and type `Windows-Features aktivieren oder deaktivieren`. In the list you have to mark the following features:
- Platform für virtuelle Computer
- Windows-Subsystem für Linux

Restart your computer after installaiton. Make sure everthing worked as planed by opening a Windows terminal and run `wsl --help`. This gives you an overview of the WSL commands. To configure WSL run `wsl --set-version 2`.


Then, go to the Microsoft Store and search for a Linux distrubtion you would like to install. A common choice is `Ubuntu 20.04 LTS`. When the download has finished execute the new Ubuntu app on your computer and choose a UNIX user and password. Alternatively you can use `wsl --install -d Ubuntu` (check for the specific version). 

## 3 Install dependencies for R and R under Linux Ubuntu

Start your Linux kernel by opening the Windows terminal or Powershell and type `wsl`. Run the following commands to install the dependencies and R via the apt Linux package manager. See https://linuxize.com/post/how-to-install-r-on-ubuntu-20-04/ as a reference.

- `sudo apt update`
- `sudo apt install dirmngr gnupg apt-transport-https ca-certificates software-properties-common`
- `sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9`
- `sudo add-apt-repository 'deb https://cloud.r-project.org/bin/linux/ubuntu focal-cran40/'`
- `sudo apt install build-essential`
- `sudo apt install libxml2-dev`
- `sudo apt install libssl-dev`
- `sudo apt install r-base`

You can test your installations by running the following commands.

- `git --version`
- `gcc -version`
- `gfortran --version`
- `R --version`

## 4 Use R to install other R packages

Open an interactive R Session by typing `sudo R`. Root access is needed to write to specific direcotries. Form here, you can use standard R commands like `install.packages("<package_name>")` to install R packages. To run the code form this repository you should run

- `install.packages("doParallel")`
- `install.packages("doMC")`
- `install.packages("tidyverse")`
- `install.packages("testit")`
- `install.packages("car")`
- `install.packages("mice")`
- `install.packages("micemd")`

## 5 Some useful (R) commands in the Linux terminal

- Execute a command as root (administrator): `sudo <command>`
- Show all files and folders in the current directory: `ls`
- Show list of system hardware: `sudo lshw`
- Change directory: `cd <path>` (You can use relative paths inside your current directory!) 
- Go back to parent directory `cd ..` 
- Go to previous directory `cd -`
- Create a new directory `mkdir <foldername>`
- Start an interactive R session: `R` (use `quit()` within R to close the session)
- Run an R script form the terminal: `Rscript <filename>` for example `Rscript src/simple_example.R`
- Interrupt the process of a running command (keyboard interrupt): `CRTL + C` (this works also inside an interactive R session)

