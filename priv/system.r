#!/usr/bin/env Rscript

# Parse the --file= argument out of command line args and
# determine where base directory is so that we can source
# our common sub-routines
arg0 <- sub("--file=(.*)", "\\1", grep("--file=", commandArgs(), value = TRUE))
dir0 <- dirname(arg0)
source(file.path(dir0, "common.r"))


theme_set(theme_grey(base_size = 17))

# Setup parameters for the script
params = matrix(c(
  'help',    'h', 0, "logical",
  'width',   'x', 2, "integer",
  'height',  'y', 2, "integer",
  'outfile', 'o', 2, "character",
  'indir',   'i', 2, "character",
  'tstart',  '1',  2, "integer",
  'tend',    '2',  2, "integer",
  'ylabel1stgraph', 'Y',  2, "character",
  'title', 't',  2, "character"
  ), ncol=4, byrow=TRUE)

# Parse the parameters
opt = getopt(params)

if (!is.null(opt$help))
  {
    cat(paste(getopt(params, command = basename(arg0), usage = TRUE)))
    q(status=1)
  }

# Initialize defaults for opt
if (is.null(opt$width))   { opt$width   = 1280 }
if (is.null(opt$height))  { opt$height  = 960 }
if (is.null(opt$indir))   { opt$indir  = "current"}
if (is.null(opt$outfile)) { opt$outfile = file.path(opt$indir, "measurement.png") }

s = load_system(opt$indir, opt$tstart, opt$tend)

if (nrow(s$cpu) == 0)
{
  stop("No latency information available to analyze in ", opt$indir)
}

png(file = opt$outfile, width = opt$width, height = opt$height)

 plot6 <- qplot(s$cpu$time, s$cpu$value, data = s$cpu,
                 geom = c("smooth", "point"),
                 xlab = "Elapsed Secs", ylab = "cpu",
                 main = NULL)

 plot7 <- qplot(s$processes$time, s$processes$value, data = s$processes,
                 geom = c("smooth", "point"),
                 xlab = "Elapsed Secs", ylab = "processes",
                 main = NULL)

 plot8 <- qplot(s$memory$time, s$memory$value, data = s$memory,
                 geom = c("smooth", "point"),
                 xlab = "Elapsed Secs", ylab = "memory",
                 main = NULL)

 plot9 <- qplot(s$filehandles$time, s$filehandles$value, data = s$filehandles,
                 geom = c("smooth", "point"),
                 xlab = "Elapsed Secs", ylab = "filehandles",
                 main = NULL)

grid.newpage()

pushViewport(viewport(layout = grid.layout(4, 1)))

vplayout <- function(x,y) viewport(layout.pos.row = x, layout.pos.col = y)

print(plot6, vp = vplayout(1,1))
print(plot7, vp = vplayout(2,1))
print(plot8, vp = vplayout(3,1))
print(plot9, vp = vplayout(4,1))

dev.off()