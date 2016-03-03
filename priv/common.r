# Load all the necessary packages, installing missing ones when necessary
packages.to.install <- c("plyr", "grid", "getopt", "proto", "ggplot2")

for(p in packages.to.install)
  {
        print(p)
        if (suppressWarnings(!require(p, character.only = TRUE))) {
            install.packages(p, repos = "http://lib.stat.cmu.edu/R/CRAN")
            library(p, character.only=TRUE)
        }
  }

# Load a latency file and ensure that it is appropriately tagged
load_latency_frame <- function(File)
  {
    op <- gsub("_latencies.csv", "", basename(File))
    frame <- read.csv(File)
    frame$op = rep(op, nrow(frame))
    return (frame)
  }

# Load summary and latency information for a given directory
load_benchmark <- function(Dir, Tstart, Tend)
  {
    ## Load up summary data
    summary <- read.csv(sprintf("%s/%s", Dir, "summary.csv"),
                        colClasses=rep("numeric", 5))

    ## Get a list of latency files
    latencies <- lapply(list.files(path = Dir, pattern = "_latencies.csv",
                                   full.names = TRUE),
                        load_latency_frame)
    latencies <- do.call('rbind', latencies)

    ## Convert timing information in latencies from usecs -> msecs
    latencies[4:10] <- latencies[4:10] / 1000

    ## Trim values off that are outside our range of times
    if (is.null(Tstart)) { Tstart = 0 }
    if (is.null(Tend)) { Tend = max(summary$elapsed) }

    print(Tstart)
    print(Tend)

    return (list(summary = summary[summary$elapsed >= Tstart & summary$elapsed <= Tend,],
                 latencies = latencies[latencies$elapsed >= Tstart & latencies$elapsed <= Tend,]))
  }


load_system <- function(Dir, Tstart, Tend)
  {
    ## Load up summary data
    cpu <- read.csv(sprintf("%s/%s", Dir, "cpu.csv"))
    memory <- read.csv(sprintf("%s/%s", Dir, "memory.csv"))
    processes <- read.csv(sprintf("%s/%s", Dir, "processes.csv"))
    filehandles <- read.csv(sprintf("%s/%s", Dir, "filehandles.csv"))

    ## Trim values off that are outside our range of times
    if (is.null(Tstart)) { Tstart = 0 }
    if (is.null(Tend)) { Tend = max(cpu$time) }

    print(Tstart)
    print(Tend)

    return (list(cpu = cpu[cpu$time >= Tstart & cpu$time <= Tend,],
                 processes = processes[processes$time >= Tstart & processes$time <= Tend,],
                 filehandles = filehandles[filehandles$time >= Tstart & filehandles$time <= Tend,],
                 memory = memory[memory$time >= Tstart & memory$time <= Tend,]))
  }
