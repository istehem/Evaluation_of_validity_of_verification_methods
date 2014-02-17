class Status:
     Ok = 1
     Deactivated = 2
     Failed = 3
     Expired = 4
     Stopped = 5

class Functions:
     GetMode = 1
     SetMode = 2
     MainFunction = 3
     CheckpointReached = 4
     PerformReset = 5
     GetFirstExpiredSeid = 6
     Init = 7
     GetLocalStatus = 8
     GetGlobalStatus = 9
     DeInit = 10
     GetVersionInfo = 11

def status_names():
    return {
        Status.Ok: 'WDGM_GLOBAL_STATUS_OK',
        Status.Deactivated: 'WDGM_GLOBAL_STATUS_DEACTIVATED',
        Status.Failed: 'WDGM_GLOBAL_STATUS_FAILED',
        Status.Expired: 'WDGM_GLOBAL_STATUS_EXPIRED',
        Status.Stopped: 'WDGM_GLOBAL_STATUS_STOPPED'
    }

def from_status_names():
  return {
      'WDGM_GLOBAL_STATUS_OK': Status.Ok,
      'WDGM_GLOBAL_STATUS_DEACTIVATED': Status.Deactivated,
      'WDGM_GLOBAL_STATUS_FAILED': Status.Failed,
      'WDGM_GLOBAL_STATUS_EXPIRED': Status.Expired,
      'WDGM_GLOBAL_STATUS_STOPPED': Status.Stopped
     }

def function_names():
    return {
        Functions.GetMode: "getmode",
        Functions.SetMode: "setmode",
        Functions.MainFunction: "mainfunction",
        Functions.CheckpointReached: "checkpointreached",
        Functions.PerformReset: "performreset",
        Functions.GetFirstExpiredSeid: "getfirstexpiredseid",
        Functions.Init: "init",
        Functions.GetLocalStatus: "getlocalstatus",
        Functions.GetGlobalStatus: "getglobalstatus",
        Functions.DeInit: "deinit",
        Functions.GetVersionInfo: "getversioninfo"
    }

def format_function(i):
     return function_names()[i]

def format_status(i):
     return status_names()[i]
