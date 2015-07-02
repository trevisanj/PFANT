module welcome
  use logging
contains
  !> Returns string representing current PFANT version

  function pfant_version() result(v)
    character(:), allocatable :: v
    v = 'v15.5.21-alpha'
  end

  !> Displays welcome message
  !>
  !> Banner created by patorjk.com Text to ASCII Art Generator (TAAG) @ref IvritFont.
  !>
  !> @note This is considered logging at INFO level; therefore nothing will be outputted
  !>       if the logging level is greater than this.
  !>
  !> @todo this is just an idea: I could make a bigger sky and select a random rectangle of it. At least 2x visible width and height to get uniform probability of given pixel being in the rectangle

  subroutine print_welcome(unit_)
    integer, intent(in) :: unit_

    if (logging_level .gt. LOGGING_INFO) return


  !  write(unit_,*) ' '
  !  write(unit_,*) ' '
  !! Shadow
  !  write(unit_,*) '   _ \   ____|  \      \  | __ __| '
  !  write(unit_,*) '  |   |  |     _ \      \ |    |   '
  !  write(unit_,*) '  ___/   __|  ___ \   |\  |    |   '
  !  write(unit_,*) ' _|     _|  _/    _\ _| \_|   _|   '
  !  write(unit_,*) '                                   '
  !  write(unit_,*) ' '
  !  write(unit_,*) ' '
  !! Italic
  !  write(unit_,*) '     ____  _________    _   ________'
  !  write(unit_,*) '    / __ \/ ____/   |  / | / /_  __/'
  !  write(unit_,*) '   / /_/ / /_  / /| | /  |/ / / /   '
  !  write(unit_,*) '  / ____/ __/ / ___ |/ /|  / / /    '
  !  write(unit_,*) ' /_/   /_/   /_/  |_/_/ |_/ /_/     '
  !  write(unit_,*) ' '
  !  write(unit_,*) ' '
  !! Glenyn
  !  write(unit_,*) ' '
  !  write(unit_,*) ' ____ ____ ___  __   ____ '
  !  write(unit_,*) ' | . \|  _\|  \ | \|\|_ _\'
  !  write(unit_,*) ' | __/| _\ | . \|  \|  || '
  !  write(unit_,*) ' |/   |/   |/\_/|/\_/  |/'
  !  write(unit_,*) ' '
  !  write(unit_,*) ' '
  !  write(unit_,*) ' '
  !  write(unit_,*) '  ____  _____ _    _   _ _____ '
  !  write(unit_,*) ' |  _ \|  ___/ \  | \ | |_   _|'
  !  write(unit_,*) ' | |_) | |_ / _ \ |  \| | | |  '
  !  write(unit_,*) ' |  __/|  _/ ___ \| |\  | | |  '
  !  write(unit_,*) ' |_|   |_|/_/   \_\_| \_| |_|  '
  !  write(unit_,*) ' '
  !  write(unit_,*) ' '



    !write(unit_,*) '________________________________________________________________________'
    !write(unit_,*) '          `                  `                   `            ``        '
    !write(unit_,*) '                                                   `    `     ``        '
    !write(unit_,*) '             `                   `             `                        '
    !write(unit_,*) '                                           ``  ``                       '
    !write(unit_,*) '   `          @.     ``             `        ``                         '
    !write(unit_,*) '                                                `                       '
    !write(unit_,*) '                                               `                        '
    !write(unit_,*) '                        @                                               '
    !write(unit_,*) '             `      `              `                 `                  '
    !write(unit_,*) '`                     `           `                                     '
    !write(unit_,*) '                  `                                                     '
    !write(unit_,*) '               @`@`@ `          `                                       '
    !write(unit_,*) '`              `    `                            `             `        '
    !write(unit_,*) '               `                                                        '
    !write(unit_,*) '              `                   ____  _____ _    _   _ _____          '
    !write(unit_,*) '               `        `        |  _ \|  ___/ \  | \ | |_   _|         '
    !write(unit_,*) '        @        `  `            | |_) | |_ / _ \ |  \| | | |         ` '
    !write(unit_,*) '                    @            |  __/|  _/ ___ \| |\  | | |           '
    !write(unit_,*) '__/\/\/\_________________________|_|___|_|/_/___\_\_|_\_|_|_|_____/\/\__'


    write(unit_,*) ''
    write(unit_,*) ''
    write(unit_,*) ''
    write(unit_,*) '`   `        `              `                                           '
    write(unit_,*) '       Welcome to PFANT      `                   `            ``        '
    write(unit_,*) '                                                   `    `     ``        '
    write(unit_,*) '             `                   `             `                        '
    write(unit_,*) '                                           ``  ``                       '
    write(unit_,*) '   `          @.     ``             `        ``                         '
    write(unit_,*) '                                                `                       '
    write(unit_,*) '                                               `                        '
    write(unit_,*) '                        @                                               '
    write(unit_,*) '             `      `              `                 `                  '
    write(unit_,*) '`                     `           `    Bugs/crashes: please report at   '
    write(unit_,*) '                  `                                                     '
    write(unit_,*) '               @`@`@ `          `      http://github.com/trevisanj/pfant'
    write(unit_,*) '`              `    `                            `             `        '
    write(unit_,*) '               `                                                        '
    write(unit_,*) '              `                     ____  _____ _    _   _ _____        '
    write(unit_,*) '               `        `          |  _ \|  ___/ \  | \ | |_   _|       '
    write(unit_,*) '        @        `  `              | |_) | |_ / _ \ |  \| | | |       ` '
    write(unit_,*) '   _                @              |  __/|  _/ ___ \| |\  | | |         '
    write(unit_,*) '__|o|______________________________|_|___|_|/_/___\_\_|_\_|_|_|_________'
    write(unit_,*) ''
    write(unit_,*) ''
    write(unit_,*) ''
  end
end


!'`   `        `              `                                           '
!'          `                  `                   `            ``        '
!'                                                   `    `     ``        '
!'             `                   `             `                        '
!'                                           ``  ``                       '
!'   `          @.     ``             `        ``                         '
!'                                                `                       '
!'                                               `                        '
!'                        @                                               '
!'             `      `              `                 `                  '
!'`                     `           `                                     '
!'                  `                                                     '
!'               @`@`@ `          `                                       '
!'`              `    `                            `             `        '
!'               `                                                        '
!'              `                                                         '
!'               `        `                                               '
!'        @        `  `                                                 ` '
!'                    @                                                   '

