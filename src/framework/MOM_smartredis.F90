!> Contains routines necessary to initialize the SmartRedis client
module MOM_smartredis

use MOM_cpu_clock,        only : cpu_clock_id, cpu_clock_begin, cpu_clock_end, CLOCK_ROUTINE
use MOM_error_handler,    only : MOM_error, FATAL, WARNING, NOTE, MOM_mesg, is_root_pe
use MOM_file_parser,      only : read_param, get_param, log_version, param_file_type
use smartredis_client,    only : client_type

implicit none; private

public :: client_type
public :: smartredis_init

contains

subroutine smartredis_init(param_file, client, client_in)
  type(param_file_type), intent(in   ) :: param_file !< Parameter file structure
  type(client_type),     intent(inout) :: client     !< Client used to communicate with the RedisAI
                                                     !! database to be stored in the MOM control structure
  type(client_type), optional, intent(in   ) :: client_in !< If present, use a previously initialized
                                                          !! SmartRedis client

  character(len=40) :: mdl = "MOM_SMARTREDIS"
  logical :: use_smartredis
  logical :: use_smartredis_cluster
  integer :: id_client_init
  integer :: return_code
  call get_param(param_file, mdl, "USE_SMARTREDIS",  use_smartredis, &
                 "If true, use the data client to connect"//&
                 "with the SmartRedis database", default=.false.)

  if (present(client_in)) then ! The driver (e.g. the NUOPC cap) has already initialized the client

    client = client_in

    if (.not. client%isinitialized() .and. use_smartredis) then
      call MOM_error(FATAL, &
      "If using a SmartRedis client not initialized within MOM, client%initialize must have already been invoked."//&
      " Check that the client has been initialized in the driver before the call to initialize_MOM")
    endif

  elseif (use_smartredis) then ! The client will be initialized within MOM

    call get_param(param_file, mdl, "USE_SMARTREDIS_CLUSTER",  use_smartredis_cluster, &
                   "If true, the SmartRedis database is distributed over multiple nodes.",&
                   default=.true.)
    id_client_init = cpu_clock_id('(SMARTREDIS client init)', grain=CLOCK_ROUTINE)
    call cpu_clock_begin(id_client_init)
    return_code = client%initialize(use_smartredis_cluster)
    if (client%SR_error_parser(return_code)) then
      call MOM_error(FATAL, "SmartRedis client failed to initialize")
    endif
    call cpu_clock_end(id_client_init)

  endif
end subroutine smartredis_init

end module MOM_smartredis

