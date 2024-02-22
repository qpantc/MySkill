FC=gfortran

TARGET = main

MAIN_SRC = main.f90
MOD_SRCS = $(wildcard mod*.f90)
MOD_OBJS = $(patsubst %.f90, %.o, $(MOD_SRCS))


all: $(TARGET)

# 自定义后缀，并且定义对所有文件进行初步编译的一般方法
# 注意：并不是所有的文件都会被编译，只有需要的才会
.SUFFIXES: .f90 .o 
.f90.o: 
	$(FC) -c $<

# 最终编译生成可执行文件
$(TARGET): $(MAIN_SRC) $(MOD_OBJS)
	$(FC) $< $(MOD_OBJS) -o $@

%.o: %.mod # module的.f90文件还会自动生成一个.mod文件

# 伪目标一般都没有依赖，既然一定要执行命令了，就不要加依赖(无条件执行) 
# 伪目标前面可以加.PHONY来声明它是伪目标。
.PHONY: all clean
clean:
	$(RM) $(TARGET) *.o *.mod