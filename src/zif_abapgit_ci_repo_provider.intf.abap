INTERFACE zif_abapgit_ci_repo_provider
  PUBLIC .


  METHODS get_repos
    RETURNING
      VALUE(rt_repos) TYPE zif_abapgit_ci_definitions=>tty_repo
    RAISING
      zcx_abapgit_exception .
ENDINTERFACE.
