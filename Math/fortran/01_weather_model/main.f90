program main

    ! =============================================================================================
    ! progame on weather in the future modelling
    ! =============================================================================================

    implicit none

    ! =============================================================================================
    ! Parameters about model running
    ! =============================================================================================
    character(80),PARAMETER :: filename = "weather.csv"

    integer,parameter :: DAY_M = 100 ! how many days you want to simulate

    real(8),parameter, dimension(3,3) :: PROB_A = reshape((/0.5, 0.25, 0.25, &      ! rain   to rain, nice, cloudy
                                                            0.5, 0.0, 0.50, &       ! nice   to rain, nice, cloudy
                                                            0.25, 0.25, 0.50/), &   ! cloudy to rain, nice, cloudy
                                                            shape(PROB_A)) ! col,row
    real(8),parameter, dimension(3) :: A_0 = [1,0,0]


    ! =============================================================================================
    ! iteration variables  
    ! =============================================================================================
    integer :: i ! 天数

    ! =============================================================================================
    ! variables the model running required
    ! =============================================================================================
    real(8), dimension(3) :: A_k0, A_k1

    ! real(8), dimension(3,DAY_M) :: A_results

    ! =============================================================================================
    ! Main codes
    ! =============================================================================================
    
    ! Write header to the CSV file
    open(unit=10, file=filename, status="replace")
    write(10, *) "Day, Rain, Nice, Cloudy"

    write(*,*) 'Day     weather probability is: Rain       Nice           Cloudy'

    A_k0 = A_0
    do i = 1,DAY_M
    
        A_k1 = matmul(PROB_A,A_k0)
        ! A_results(:,i) = A_k1
        write(*,*) i,'weather probability is:', A_k1
        write(10, "((I4),3(',',F5.4))") i,A_k1

        A_k0 = A_k1
    end do

    close(10)

    ! write(*,*) A_results
end program main

