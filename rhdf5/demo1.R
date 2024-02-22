library(rhdf5)


# Creating an HDF5 file and group hierarchy -----------------------------------------------------------------------
# create an empty HDF5 file
h5createFile("myhdf5file.h5")

# Create groups
# The HDF5 file can contain a group hierarchy. 
# We create a number of groups and 
h5createGroup("myhdf5file.h5","foo")
h5createGroup("myhdf5file.h5","baa")
h5createGroup("myhdf5file.h5","foo/foobaa")

# list the file content
h5ls("myhdf5file.h5")



# Writing and reading objects -------------------------------------------------------------------------------------
A = matrix(1:10,nr=5,nc=2)
h5write(A, "myhdf5file.h5","foo/A")
B = array(seq(0.1,2.0,by=0.1),dim=c(5,2,2))
attr(B, "scale") <- "liter"
h5write(B, "myhdf5file.h5","foo/B")
C = matrix(paste(LETTERS[1:10],LETTERS[11:20], collapse=""),
           nr=2,nc=5)
h5write(C, "myhdf5file.h5","foo/foobaa/C")
df = data.frame(1L:5L,seq(0,1,length.out=5),
                c("ab","cde","fghi","a","s"), stringsAsFactors=FALSE)
h5write(df, "myhdf5file.h5","df")
h5ls("myhdf5file.h5")


D = h5read("myhdf5file.h5","foo/A")
E = h5read("myhdf5file.h5","foo/B")
F = h5read("myhdf5file.h5","foo/foobaa/C")
G = h5read("myhdf5file.h5","df")

# Writing and reading objects with file, group and dataset handles ------------------------------------------------
h5f = H5Fopen("myhdf5file.h5")
h5f
h5f$df
h5f&'df'
h5f$foo$foobaa$C
h5f$"/foo/foobaa/C"
h5d = h5f&"/foo"
