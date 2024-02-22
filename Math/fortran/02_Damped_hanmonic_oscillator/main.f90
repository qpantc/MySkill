program main

    ! =============================================================================================
    ! Using a secondary order system to slove the damped hanmonic oscillator
    ! ---------------------------------------------------------------------------------------------
    ! Breif decribtion: mx'' + dx' + kx = 0 --> x'' + dx'/m + kx/m = 0
    ! m: is the mass of the objective; d is the damper efficiency; k is the sprinkle efficiency
    ! the Matrix:
    ! d[x,v]/dt = d[0   ,     1]     *   [x,    
    !          [-k/m,  -d/m]/dt       v]
    ! =============================================================================================

    implicit none
    
    ! =============================================================================================
    ! constant and paramters
    ! =============================================================================================
    real(8),PARAMETER :: PI = 3.141592653
    INTEGER,PARAMETER :: MAX_TIME = 100
    real(8),PARAMETER :: DT = 0.01
    character(80),PARAMETER :: filename = "X_V.csv"


    ! =============================================================================================
    ! setting about the model running
    ! m: kg, k: N/m, d: ununit
    ! initial status: 
    ! let the sprinkle k/m = w^2, and w = 2pi, natural frequency, 
    ! the damper: d/m = d*w^2/k
    ! d = 0.25, and d/m = d*w
    ! -w^2
    ! let k= 1 k/m = w^2  d/m = 
    ! =============================================================================================
    real(8),PARAMETER :: mass = 1.0, k=10.0, d = 0.25
    real(8),PARAMETER :: X0 = 10.0, V0 = 0.0

    ! =============================================================================================
    ! model variables and loops i,j
    ! =============================================================================================
    real(8), dimension(2,2) :: A
    real(8), dimension(2) :: XV
    real(8) :: time_i
    INTEGER :: i,j

    ! =============================================================================================
    ! main running codes
    ! =============================================================================================

    open(unit=10, file=filename, status="replace")
    write(10, *) "Time,X,V"

    XV = [X0,V0]

    A(1,1) = 0.0
    A(2,1) = 1.0
    A(1,2) = -k/mass
    A(2,2) = -d/mass
    write(*,"('initial position is:',(F6.2),'   initial velocity is:', (F6.2))")  XV(1),XV(2),A

    do i = 1, int(MAX_TIME/DT)
        time_i = i * DT
        XV = XV + matmul(A,XV)*DT
        write(10, "((F8.4),2(',',F8.4))") time_i,XV
        write(*,*) XV

    end do


end program main
