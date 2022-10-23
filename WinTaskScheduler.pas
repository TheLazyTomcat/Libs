{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  WinTaskScheduler

    This unit provides types, constants and most importantly interfaces for
    Windows Task Scheduler. Interfaces for both Task Scheduler 1.0 and
    Task Scheduler 2.0 are included.
    TS 1.0 should be used only on Windows 2000, XP and Server 2003, it is
    deprecated since Windows Vista and should not be used there (although
    it seems to work for now).
    TS 2.0 is available only from Windows Vista up.

  version 1.0 (2017-08-25)

  Last change 2020-08-02

  ©2017-2020 František Milt

  Contacts:
    František Milt: frantisek.milt@gmail.com

  Support:
    If you find this code useful, please consider supporting its author(s) by
    making a small donation using the following link(s):

      https://www.paypal.me/FMilt

  Changelog:
    For detailed changelog and history please refer to this git repository:

      github.com/TheLazyTomcat/Bnd.WinTaskScheduler

  Dependencies:
    none

===============================================================================}
unit WinTaskScheduler;

{$IF not(defined(MSWINDOWS) or defined(WINDOWS))}
  {$MESSAGE FATAL 'Unsupported operating system.'}
{$IFEND}

{$IFDEF FPC}
  {$MODE ObjFPC}
{$ENDIF}
{$H+}

{$MINENUMSIZE 4}

interface

uses
  Windows, ActiveX;

{===============================================================================

    Basic types
    
===============================================================================}

type
  INT  = Integer;
  LONG = LongInt;

  VARIANT_BOOL = WordBool;

  PLPWSTR  = ^LPWSTR;
  PPLPWSTR = ^PLPWSTR;

  BSTR  = PWideChar;
  PBSTR = ^BSTR;

  DATE = TDateTime;

  HPROPSHEETPAGE = THandle;
  PHPROPSHEETPAGE = ^HPROPSHEETPAGE;

  LPSYSTEMTIME = ^SYSTEMTIME;
  PLPSYSTEMTIME = ^LPSYSTEMTIME;

  PHRESULT = ^HRESULT;

  PPBYTE = ^PBYTE;

  REFIID   = ^TGUID;
  REFCLSID = ^TGUID;

  PIUnknown = ^IUnknown;


{===============================================================================
--------------------------------------------------------------------------------

    Task Scheduler 1.0 (MSTask.h, MSTask.idl)

--------------------------------------------------------------------------------
===============================================================================}

{===============================================================================

    Datatypes

===============================================================================}

const
  TASK_SUNDAY      = $1;
  TASK_MONDAY      = $2;
  TASK_TUESDAY     = $4;
  TASK_WEDNESDAY   = $8;
  TASK_THURSDAY    = $10;
  TASK_FRIDAY      = $20;
  TASK_SATURDAY    = $40;
  TASK_FIRST_WEEK  = 1;
  TASK_SECOND_WEEK = 2;
  TASK_THIRD_WEEK  = 3;
  TASK_FOURTH_WEEK = 4;
  TASK_LAST_WEEK   = 5;
  TASK_JANUARY     = $1;
  TASK_FEBRUARY    = $2;
  TASK_MARCH       = $4;
  TASK_APRIL       = $8;
  TASK_MAY         = $10;
  TASK_JUNE        = $20;
  TASK_JULY        = $40;
  TASK_AUGUST      = $80;
  TASK_SEPTEMBER   = $100;
  TASK_OCTOBER     = $200;
  TASK_NOVEMBER    = $400;
  TASK_DECEMBER    = $800;

  TASK_FLAG_INTERACTIVE                  = $1;
  TASK_FLAG_DELETE_WHEN_DONE             = $2;
  TASK_FLAG_DISABLED                     = $4;
  TASK_FLAG_START_ONLY_IF_IDLE           = $10;
  TASK_FLAG_KILL_ON_IDLE_END             = $20;
  TASK_FLAG_DONT_START_IF_ON_BATTERIES   = $40;
  TASK_FLAG_KILL_IF_GOING_ON_BATTERIES   = $80;
  TASK_FLAG_RUN_ONLY_IF_DOCKED           = $100;
  TASK_FLAG_HIDDEN                       = $200;
  TASK_FLAG_RUN_IF_CONNECTED_TO_INTERNET = $400;
  TASK_FLAG_RESTART_ON_IDLE_RESUME       = $800;
  TASK_FLAG_SYSTEM_REQUIRED              = $1000;
  TASK_FLAG_RUN_ONLY_IF_LOGGED_ON        = $2000;

  TASK_TRIGGER_FLAG_HAS_END_DATE         = $1;
  TASK_TRIGGER_FLAG_KILL_AT_DURATION_END = $2;
  TASK_TRIGGER_FLAG_DISABLED             = $4;

{
  1440 = 60 mins/hour * 24 hrs/day since a trigger/TASK could run all day at
  one minute intervals.
}
  TASK_MAX_RUN_TIMES = 1440;

//==============================================================================

type
{
  The TASK_TRIGGER_TYPE field of the TASK_TRIGGER structure determines
  which member of the TRIGGER_TYPE_UNION field to use.

  https://msdn.microsoft.com/library/windows/desktop/aa383620
}
  _TASK_TRIGGER_TYPE =(
    TASK_TIME_TRIGGER_ONCE,             // Ignore the Type field.
    TASK_TIME_TRIGGER_DAILY,            // Use DAILY
    TASK_TIME_TRIGGER_WEEKLY,           // Use WEEKLY
    TASK_TIME_TRIGGER_MONTHLYDATE,      // Use MONTHLYDATE
    TASK_TIME_TRIGGER_MONTHLYDOW,       // Use MONTHLYDOW
    TASK_EVENT_TRIGGER_ON_IDLE,         // Ignore the Type field.
    TASK_EVENT_TRIGGER_AT_SYSTEMSTART,  // Ignore the Type field.
    TASK_EVENT_TRIGGER_AT_LOGON);       // Ignore the Type field.
  TASK_TRIGGER_TYPE = _TASK_TRIGGER_TYPE;
  PTASK_TRIGGER_TYPE = ^TASK_TRIGGER_TYPE;

{
  https://msdn.microsoft.com/library/windows/desktop/aa446857
}
  _DAILY = record
    DaysInterval: WORD;
  end;
  DAILY = _DAILY;

{
  https://msdn.microsoft.com/library/windows/desktop/aa384014
}
  _WEEKLY = record
    WeeksInterval:    WORD;
    rgfDaysOfTheWeek: WORD;
  end;
  WEEKLY = _WEEKLY;

{
  https://msdn.microsoft.com/library/windows/desktop/aa381918
}
  _MONTHLYDATE = record
    rgfDays:    DWORD;
    rgfMonths:  WORD;
  end;
  MONTHLYDATE = _MONTHLYDATE;

{
  https://msdn.microsoft.com/library/windows/desktop/aa381950
}
  _MONTHLYDOW = record
    wWhichWeek:        WORD;
    rgfDaysOfTheWeek:  WORD;
    rgfMonths:         WORD;
  end;
  MONTHLYDOW = _MONTHLYDOW;

{
  https://msdn.microsoft.com/library/windows/desktop/aa384002
}
  _TRIGGER_TYPE_UNION = record
  case Integer of
    0: (Daily:        DAILY);
    1: (Weekly:       WEEKLY);
    2: (MonthlyDate:  MONTHLYDATE);
    3: (MonthlyDOW:   MONTHLYDOW);
  end;
  TRIGGER_TYPE_UNION = _TRIGGER_TYPE_UNION;

{
  https://msdn.microsoft.com/library/windows/desktop/aa383618
}
  _TASK_TRIGGER = record
    cbTriggerSize:          WORD;               // Structure size.
    Reserved1:              WORD;               // Reserved. Must be zero.
    wBeginYear:             WORD;               // Trigger beginning date year.
    wBeginMonth:            WORD;               // Trigger beginning date month.
    wBeginDay:              WORD;               // Trigger beginning date day.
    wEndYear:               WORD;               // Optional trigger ending date year.
    wEndMonth:              WORD;               // Optional trigger ending date month.
    wEndDay:                WORD;               // Optional trigger ending date day.
    wStartHour:             WORD;               // Run bracket start time hour.
    wStartMinute:           WORD;               // Run bracket start time minute.
    MinutesDuration:        DWORD;              // Duration of run bracket.
    MinutesInterval:        DWORD;              // Run bracket repetition interval.
    rgFlags:                DWORD;              // Trigger flags.
    TriggerType:            TASK_TRIGGER_TYPE;  // Trigger type.
    // "Type" is reserved word in pascal...
    Type_:                  TRIGGER_TYPE_UNION; // Trigger data.
    Reserved2:              WORD;               // Reserved. Must be zero.
    wRandomMinutesInterval: WORD;               // Maximum number of random minutes after start time.
  end;
  TASK_TRIGGER = _TASK_TRIGGER;
  PTASK_TRIGGER = ^TASK_TRIGGER;


{===============================================================================

    Interfaces

===============================================================================}

const
  IID_ITaskTrigger:       TGUID = '{148BD52B-A2AB-11CE-B11F-00AA00530503}';
  IID_IScheduledWorkItem: TGUID = '{a6b952f0-a4b1-11d0-997d-00aa006887ec}';
  IID_ITask:              TGUID = '{148BD524-A2AB-11CE-B11F-00AA00530503}';
  IID_IEnumWorkItems:     TGUID = '{148BD528-A2AB-11CE-B11F-00AA00530503}';
  IID_ITaskScheduler:     TGUID = '{148BD527-A2AB-11CE-B11F-00AA00530503}';
  IID_IProvideTaskPage:   TGUID = '{4086658a-cbbb-11cf-b604-00c04fd8d565}';

{-------------------------------------------------------------------------------

    Interface:  ITaskTrigger

    Synopsis:   Trigger object interface. A Task object may contain several
                of these.

-------------------------------------------------------------------------------}
{
  https://msdn.microsoft.com/library/windows/desktop/aa381864
}
type
  PITaskTrigger = ^ITaskTrigger;
  ITaskTrigger = interface(IUnknown)
  ['{148BD52B-A2AB-11CE-B11F-00AA00530503}']
    Function SetTrigger(const pTrigger: PTASK_TRIGGER): HRESULT; stdcall;
    Function GetTrigger(pTrigger: PTASK_TRIGGER): HRESULT; stdcall;
    Function GetTriggerString(ppwszTrigger: PLPWSTR): HRESULT; stdcall;
  end;

{-------------------------------------------------------------------------------

    Interface:  IScheduledWorkItem

    Synopsis:   Abstract base class for any runnable work item that can be
                scheduled by the task scheduler.

-------------------------------------------------------------------------------}
{
  https://msdn.microsoft.com/library/windows/desktop/aa381216
}
  IScheduledWorkItem = interface(IUnknown)
  ['{a6b952f0-a4b1-11d0-997d-00aa006887ec}']
    Function CreateTrigger(piNewTrigger: PWORD; ppTrigger: PITaskTrigger): HRESULT; stdcall;
    Function DeleteTrigger(iTrigger: WORD): HRESULT; stdcall;
    Function GetTriggerCount(plCount: PWORD): HRESULT; stdcall;
    Function GetTrigger(iTrigger: WORD; ppTrigger: PITaskTrigger): HRESULT; stdcall;
    Function GetTriggerString(iTrigger: WORD; ppwszTrigger: PLPWSTR): HRESULT; stdcall;
    Function GetRunTimes(pstBegin: LPSYSTEMTIME; pstEnd: LPSYSTEMTIME; pCount: PWORD; rgstTaskTimes: PLPSYSTEMTIME): HRESULT; stdcall;
    Function GetNextRunTime(pstNextRun: PSYSTEMTIME): HRESULT; stdcall;
    Function SetIdleWait(wIdleMinutes: WORD; wDeadlineMinutes: WORD): HRESULT; stdcall;
    Function GetIdleWait(pwIdleMinutes: PWORD; pwDeadlineMinutes: PWORD): HRESULT; stdcall;
    Function Run: HRESULT; stdcall;
    Function Terminate: HRESULT; stdcall;
    Function EditWorkItem(hParent: HWND; dwReserved: DWORD): HRESULT; stdcall;
    Function GetMostRecentRunTime(pstLastRun: PSYSTEMTIME): HRESULT; stdcall;
    Function GetStatus(phrStatus: PHRESULT): HRESULT; stdcall;
    Function GetExitCode(pdwExitCode: PDWORD): HRESULT; stdcall;
    Function SetComment(pwszComment: LPCWSTR): HRESULT; stdcall;
    Function GetComment(ppwszComment: PLPWSTR): HRESULT; stdcall;
    Function SetCreator(pwszCreator: LPCWSTR): HRESULT; stdcall;
    Function GetCreator(ppwszCreator: PLPWSTR): HRESULT; stdcall;
    // rgbData is actually pointer to array of bytes
    Function SetWorkItemData(cBytes: WORD; rgbData: PBYTE): HRESULT; stdcall;
    Function GetWorkItemData(pcBytes: PWORD; ppBytes: PPBYTE): HRESULT; stdcall;
    Function SetErrorRetryCount(wRetryCount: WORD): HRESULT; stdcall;
    Function GetErrorRetryCount(pwRetryCount: PWORD): HRESULT; stdcall;
    Function SetErrorRetryInterval(wRetryInterval: WORD): HRESULT; stdcall;
    Function GetErrorRetryInterval(pwRetryInterval: PWORD): HRESULT; stdcall;
    Function SetFlags(dwFlags: DWORD): HRESULT; stdcall;
    Function GetFlags(pdwFlags: PDWORD): HRESULT; stdcall;
    Function SetAccountInformation(pwszAccountName: LPCWSTR;pwszPassword: LPCWSTR): HRESULT; stdcall;
    Function GetAccountInformation(ppwszAccountName: PLPWSTR): HRESULT; stdcall;
  end;

{-------------------------------------------------------------------------------

    Interface:  ITask

    Synopsis:   Task object interface. The primary means of task object
                manipulation.

-------------------------------------------------------------------------------}
{
  https://msdn.microsoft.com/library/windows/desktop/aa381311
}
  ITask = interface(IScheduledWorkItem)
  ['{148BD524-A2AB-11CE-B11F-00AA00530503}']
    Function SetApplicationName(pwszApplicationName: LPCWSTR): HRESULT; stdcall;
    Function GetApplicationName(ppwszApplicationName: PLPWSTR): HRESULT; stdcall;
    Function SetParameters(pwszParameters: LPCWSTR): HRESULT; stdcall;
    Function GetParameters(ppwszParameters: PLPWSTR): HRESULT; stdcall;
    Function SetWorkingDirectory(pwszWorkingDirectory: LPCWSTR): HRESULT; stdcall;
    Function GetWorkingDirectory(ppwszWorkingDirectory: PLPWSTR): HRESULT; stdcall;
    Function SetPriority(dwPriority: DWORD): HRESULT; stdcall;
    Function GetPriority(pdwPriority: PDWORD): HRESULT; stdcall;
    Function SetTaskFlags(dwFlags: DWORD): HRESULT; stdcall;
    Function GetTaskFlags(pdwFlags: PDWORD): HRESULT; stdcall;
    Function SetMaxRunTime(dwMaxRunTime: DWORD): HRESULT; stdcall;
    Function GetMaxRunTime(pdwMaxRunTime: PDWORD): HRESULT; stdcall;
  end;

{-------------------------------------------------------------------------------

    Interface:  IEnumWorkItems

    Synopsis:   Work item object enumerator. Enumerates the work item objects
                within the Tasks folder.

-------------------------------------------------------------------------------}
{
  https://msdn.microsoft.com/library/windows/desktop/aa380706
}
  PIEnumWorkItems = ^IEnumWorkItems;
  IEnumWorkItems = interface(IUnknown)
  ['{148BD528-A2AB-11CE-B11F-00AA00530503}']
    Function Next(celt: ULONG; rgpwszNames: PPLPWSTR; pceltFetched: PULONG): HRESULT; stdcall;
    Function Skip(celt: ULONG): HRESULT; stdcall;
    Function Reset: HRESULT; stdcall;
    Function Clone(ppEnumWorkItems: PIEnumWorkItems): HRESULT; stdcall;
  end;

{-------------------------------------------------------------------------------

    Interface:  ITaskScheduler

    Synopsis:   Task Scheduler interface. Provides location transparent
                manipulation of task and/or queue objects within the Tasks
                folder.

-------------------------------------------------------------------------------}
{
  https://msdn.microsoft.com/library/windows/desktop/aa381811
}
  ITaskScheduler = interface(IUnknown)
  ['{148BD527-A2AB-11CE-B11F-00AA00530503}']
    Function SetTargetComputer(pwszComputer: LPCWSTR): HRESULT; stdcall;
    Function GetTargetComputer(ppwszComputer: PLPWSTR): HRESULT; stdcall;
    Function Enum(ppEnumTasks: PIEnumWorkItems): HRESULT; stdcall;
    Function Activate(pwszName: LPCWSTR; riid: REFIID; ppunk: PIUnknown): HRESULT; stdcall;
    Function Delete(pwszName: LPCWSTR): HRESULT; stdcall;
    Function NewWorkItem(pwszTaskName: LPCWSTR; rclsid: REFCLSID; riid: REFIID; ppunk: PIUnknown): HRESULT; stdcall;
    Function AddWorkItem(pwszTaskName: LPCWSTR; pWorkItem: IScheduledWorkItem): HRESULT; stdcall;
    Function IsOfType(pwszName: LPCWSTR; riid: REFIID): HRESULT; stdcall;
  end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

const
  CLSID_CTask:          TGUID = '{148BD520-A2AB-11CE-B11F-00AA00530503}';
  CLSID_CTaskScheduler: TGUID = '{148BD52A-A2AB-11CE-B11F-00AA00530503}';

type
{
  https://msdn.microsoft.com/library/windows/desktop/aa382588
}
  _TASKPAGE = (
    TASKPAGE_TASK,
    TASKPAGE_SCHEDULE,
    TASKPAGE_SETTINGS);
  TASKPAGE = _TASKPAGE;

{-------------------------------------------------------------------------------

    Interface:  IProvideTaskPage

    Synopsis:   Task property page retrieval interface. With this interface,
                it is possible to retrieve one or more property pages
                associated with a task object. Task objects inherit this
                interface.

-------------------------------------------------------------------------------}
{
  https://msdn.microsoft.com/library/windows/desktop/aa380749
}
  IProvideTaskPage = interface(IUnknown)
  ['{4086658a-cbbb-11cf-b604-00c04fd8d565}']
    Function GetPage(tpType: TASKPAGE; fPersistChanges: BOOL; phPage: PHPROPSHEETPAGE): HRESULT; stdcall;
  end;

//------------------------------------------------------------------------------

type
  ISchedulingAgent = ITaskScheduler;
  IEnumTasks = IEnumWorkItems;

const
  IID_IPersistFile:       TGUID = '{0000010b-0000-0000-C000-000000000046}';
  IID_ISchedulingAgent:   TGUID = '{148BD527-A2AB-11CE-B11F-00AA00530503}';
  CLSID_CSchedulingAgent: TGUID = '{148BD52A-A2AB-11CE-B11F-00AA00530503}';


{===============================================================================
--------------------------------------------------------------------------------

    Task Scheduler 2.0 (taskschd.h)

--------------------------------------------------------------------------------
===============================================================================}

{===============================================================================

    Datatypes

===============================================================================}

const
{
  https://msdn.microsoft.com/library/windows/desktop/aa383604
}
  SCHED_S_TASK_READY                  = $00041300;    // The task is ready to run at its next scheduled time.
  SCHED_S_TASK_RUNNING                = $00041301;    // The task is currently running.
  SCHED_S_TASK_DISABLED               = $00041302;    // The task will not run at the scheduled times because it has been disabled.
  SCHED_S_TASK_HAS_NOT_RUN            = $00041303;    // The task has not yet run.
  SCHED_S_TASK_NO_MORE_RUNS           = $00041304;    // There are no more runs scheduled for this task.
  SCHED_S_TASK_NOT_SCHEDULED          = $00041305;    // One or more of the properties that are needed to run this task on a schedule have not been set.
  SCHED_S_TASK_TERMINATED             = $00041306;    // The last run of the task was terminated by the user.
  SCHED_S_TASK_NO_VALID_TRIGGERS      = $00041307;    // Either the task has no triggers or the existing triggers are disabled or not set.
  SCHED_S_EVENT_TRIGGER               = $00041308;    // Event triggers do not have set run times.
  SCHED_E_TRIGGER_NOT_FOUND           = $80041309;    // A task's trigger is not found.
  SCHED_E_TASK_NOT_READY              = $8004130A;    // One or more of the properties required to run this task have not been set.
  SCHED_E_TASK_NOT_RUNNING            = $8004130B;    // There is no running instance of the task.
  SCHED_E_SERVICE_NOT_INSTALLED       = $8004130C;    // The Task Scheduler service is not installed on this computer.
  SCHED_E_CANNOT_OPEN_TASK            = $8004130D;    // The task object could not be opened.
  SCHED_E_INVALID_TASK                = $8004130E;    // The object is either an invalid task object or is not a task object.
  SCHED_E_ACCOUNT_INFORMATION_NOT_SET = $8004130F;    // No account information could be found in the Task Scheduler security database for the task indicated.
  SCHED_E_ACCOUNT_NAME_NOT_FOUND      = $80041310;    // Unable to establish existence of the account specified.
  SCHED_E_ACCOUNT_DBASE_CORRUPT       = $80041311;    // Corruption was detected in the Task Scheduler security database; the database has been reset.
  SCHED_E_NO_SECURITY_SERVICES        = $80041312;    // Task Scheduler security services are available only on Windows NT.
  SCHED_E_UNKNOWN_OBJECT_VERSION      = $80041313;    // The task object version is either unsupported or invalid.
  SCHED_E_UNSUPPORTED_ACCOUNT_OPTION  = $80041314;    // The task has been configured with an unsupported combination of account settings and run time options.
  SCHED_E_SERVICE_NOT_RUNNING         = $80041315;    // The Task Scheduler Service is not running.
  SCHED_E_UNEXPECTEDNODE              = $80041316;    // The task XML contains an unexpected node.
  SCHED_E_NAMESPACE                   = $80041317;    // The task XML contains an element or attribute from an unexpected namespace.
  SCHED_E_INVALIDVALUE                = $80041318;    // The task XML contains a value which is incorrectly formatted or out of range.
  SCHED_E_MISSINGNODE                 = $80041319;    // The task XML is missing a required element or attribute.
  SCHED_E_MALFORMEDXML                = $8004131A;    // The task XML is malformed.
  SCHED_S_SOME_TRIGGERS_FAILED        = $0004131B;    // The task is registered, but not all specified triggers will start the task.
  SCHED_S_BATCH_LOGON_PROBLEM         = $0004131C;    // The task is registered, but may fail to start. Batch logon privilege needs to be enabled for the task principal.
  SCHED_E_TOO_MANY_NODES              = $8004131D;    // The task XML contains too many nodes of the same type.
  SCHED_E_PAST_END_BOUNDARY           = $8004131E;    // The task cannot be started after the trigger end boundary.
  SCHED_E_ALREADY_RUNNING             = $8004131F;    // An instance of this task is already running.
  SCHED_E_USER_NOT_LOGGED_ON          = $80041320;    // The task will not run because the user is not logged on.
  SCHED_E_INVALID_TASK_HASH           = $80041321;    // The task image is corrupt or has been tampered with.
  SCHED_E_SERVICE_NOT_AVAILABLE       = $80041322;    // The Task Scheduler service is not available.
  SCHED_E_SERVICE_TOO_BUSY            = $80041323;    // The Task Scheduler service is too busy to handle your request. Please try again later.
  SCHED_E_TASK_ATTEMPTED              = $80041324;    // The Task Scheduler service attempted to run the task, but the task did not run due to one of the constraints in the task definition.
  SCHED_S_TASK_QUEUED                 = $00041325;    // The Task Scheduler service has asked the task to run.
  SCHED_E_TASK_DISABLED               = $80041326;    // The task is disabled.
  SCHED_E_TASK_NOT_V1_COMPAT          = $80041327;    // The task has properties that are not compatible with earlier versions of Windows.
  SCHED_E_START_ON_DEMAND             = $80041328;    // The task settings do not allow the task to start on demand.

//==============================================================================

type
{
  https://msdn.microsoft.com/library/windows/desktop/aa383574
}
  _TASK_RUN_FLAGS = (
    TASK_RUN_NO_FLAGS           = 0,
    TASK_RUN_AS_SELF            = $1,
    TASK_RUN_IGNORE_CONSTRAINTS	= $2,
    TASK_RUN_USE_SESSION_ID     = $4,
    TASK_RUN_USER_SID           = $8);
  TASK_RUN_FLAGS = _TASK_RUN_FLAGS;

{
  https://msdn.microsoft.com/library/windows/desktop/aa383558
}
  _TASK_ENUM_FLAGS = (
    TASK_ENUM_HIDDEN = $1);
  TASK_ENUM_FLAGS = _TASK_ENUM_FLAGS;

{
  https://msdn.microsoft.com/library/windows/desktop/aa383566
}
  _TASK_LOGON_TYPE = (
    TASK_LOGON_NONE,
    TASK_LOGON_PASSWORD,
    TASK_LOGON_S4U,
    TASK_LOGON_INTERACTIVE_TOKEN,
    TASK_LOGON_GROUP,
    TASK_LOGON_SERVICE_ACCOUNT,
    TASK_LOGON_INTERACTIVE_TOKEN_OR_PASSWORD);
  TASK_LOGON_TYPE = _TASK_LOGON_TYPE;

{
  https://msdn.microsoft.com/library/windows/desktop/aa383572
}
  _TASK_RUNLEVEL = (
    TASK_RUNLEVEL_LUA,
    TASK_RUNLEVEL_HIGHEST);
  TASK_RUNLEVEL_TYPE = _TASK_RUNLEVEL;

{
  https://msdn.microsoft.com/library/windows/desktop/ee695874
}
  _TASK_PROCESSTOKENSID = (
    TASK_PROCESSTOKENSID_NONE,
    TASK_PROCESSTOKENSID_UNRESTRICTED,
    TASK_PROCESSTOKENSID_DEFAULT);
  TASK_PROCESSTOKENSID_TYPE = _TASK_PROCESSTOKENSID;

{
  https://msdn.microsoft.com/library/windows/desktop/aa383617
}
  _TASK_STATE = (
    TASK_STATE_UNKNOWN,
    TASK_STATE_DISABLED,
    TASK_STATE_QUEUED,
    TASK_STATE_READY,
    TASK_STATE_RUNNING);
  TASK_STATE = _TASK_STATE;

{
  https://msdn.microsoft.com/library/windows/desktop/aa382538
}
  _TASK_CREATION = (
    TASK_VALIDATE_ONLY                = $1,
    TASK_CREATE                       = $2,
    TASK_UPDATE                       = $4,
    TASK_CREATE_OR_UPDATE             = LongWord(TASK_CREATE) or
                                        LongWord(TASK_UPDATE),
    TASK_DISABLE                      = $8,
    TASK_DONT_ADD_PRINCIPAL_ACE       = $10,
    TASK_IGNORE_REGISTRATION_TRIGGERS	= $20);
  TASK_CREATION = _TASK_CREATION;

{
  https://msdn.microsoft.com/library/windows/desktop/aa383915
}
  _TASK_TRIGGER_TYPE2 = (
    TASK_TRIGGER_EVENT                = 0,
    TASK_TRIGGER_TIME                 = 1,
    TASK_TRIGGER_DAILY                = 2,
    TASK_TRIGGER_WEEKLY               = 3,
    TASK_TRIGGER_MONTHLY              = 4,
    TASK_TRIGGER_MONTHLYDOW           = 5,
    TASK_TRIGGER_IDLE	                = 6,
    TASK_TRIGGER_REGISTRATION         = 7,
    TASK_TRIGGER_BOOT                 = 8,
    TASK_TRIGGER_LOGON                = 9,
    TASK_TRIGGER_SESSION_STATE_CHANGE = 11,
    TASK_TRIGGER_CUSTOM_TRIGGER_01    = 12);
  TASK_TRIGGER_TYPE2 = _TASK_TRIGGER_TYPE2;

{
  https://msdn.microsoft.com/library/windows/desktop/aa383616
}
  _TASK_SESSION_STATE_CHANGE_TYPE = (
    TASK_CONSOLE_CONNECT    = 1,
    TASK_CONSOLE_DISCONNECT = 2,
    TASK_REMOTE_CONNECT     = 3,
    TASK_REMOTE_DISCONNECT  = 4,
    TASK_SESSION_LOCK       = 7,
    TASK_SESSION_UNLOCK     = 8);
  TASK_SESSION_STATE_CHANGE_TYPE = _TASK_SESSION_STATE_CHANGE_TYPE;

{
  https://msdn.microsoft.com/library/windows/desktop/aa383553
}
  _TASK_ACTION_TYPE = (
    TASK_ACTION_EXEC          = 0,
    TASK_ACTION_COM_HANDLER   = 5,
    TASK_ACTION_SEND_EMAIL    = 6,
    TASK_ACTION_SHOW_MESSAGE  = 7);
  TASK_ACTION_TYPE = _TASK_ACTION_TYPE;

{
  https://msdn.microsoft.com/library/windows/desktop/aa383563
}
  _TASK_INSTANCES_POLICY = (
    TASK_INSTANCES_PARALLEL,
    TASK_INSTANCES_QUEUE,
    TASK_INSTANCES_IGNORE_NEW,
    TASK_INSTANCES_STOP_EXISTING);
  TASK_INSTANCES_POLICY = _TASK_INSTANCES_POLICY;

{
  https://msdn.microsoft.com/library/windows/desktop/aa383557
}
  _TASK_COMPATIBILITY = (
    TASK_COMPATIBILITY_AT,
    TASK_COMPATIBILITY_V1,
    TASK_COMPATIBILITY_V2,
    TASK_COMPATIBILITY_V2_1,
    TASK_COMPATIBILITY_V2_2,
    TASK_COMPATIBILITY_V2_3);
  TASK_COMPATIBILITY = _TASK_COMPATIBILITY;

  
{===============================================================================

    Interfaces

===============================================================================}

const
  IID_IRunningTask:               TGUID = '{653758fb-7b9a-4f1e-a471-beeb8e9b834e}';
  IID_IRunningTaskCollection:     TGUID = '{6a67614b-6828-4fec-aa54-6d52e8f1f2db}';
  IID_IRegistrationInfo:          TGUID = '{416D8B73-CB41-4ea1-805C-9BE9A5AC4A74}';
  IID_IRepetitionPattern:         TGUID = '{7FB9ACF1-26BE-400e-85B5-294B9C75DFD6}';
  IID_ITaskNamedValuePair:        TGUID = '{39038068-2B46-4afd-8662-7BB6F868D221}';
  IID_ITaskNamedValueCollection:  TGUID = '{B4EF826B-63C3-46e4-A504-EF69E4F7EA4D}';
  IID_ITrigger:                   TGUID = '{09941815-ea89-4b5b-89e0-2a773801fac3}';
  IID_ITriggerCollection:         TGUID = '{85df5081-1b24-4f32-878a-d9d14df4cb77}';
  IID_IBootTrigger:               TGUID = '{2A9C35DA-D357-41f4-BBC1-207AC1B1F3CB}';
  IID_IDailyTrigger:              TGUID = '{126c5cd8-b288-41d5-8dbf-e491446adc5c}';
  IID_IEventTrigger:              TGUID = '{d45b0167-9653-4eef-b94f-0732ca7af251}';
  IID_IIdleTrigger:               TGUID = '{d537d2b0-9fb3-4d34-9739-1ff5ce7b1ef3}';
  IID_ILogonTrigger:              TGUID = '{72DADE38-FAE4-4b3e-BAF4-5D009AF02B1C}';
  IID_IMonthlyDOWTrigger:         TGUID = '{77d025a3-90fa-43aa-b52e-cda5499b946a}';
  IID_IMonthlyTrigger:            TGUID = '{97c45ef1-6b02-4a1a-9c0e-1ebfba1500ac}';
  IID_IRegistrationTrigger:       TGUID = '{4c8fec3a-c218-4e0c-b23d-629024db91a2}';
  IID_ISessionStateChangeTrigger: TGUID = '{754DA71B-4385-4475-9DD9-598294FA3641}';
  IID_ITimeTrigger:               TGUID = '{b45747e0-eba7-4276-9f29-85c5bb300006}';
  IID_IWeeklyTrigger:             TGUID = '{5038fc98-82ff-436d-8728-a512a57c9dc1}';
  IID_IIdleSettings:              TGUID = '{84594461-0053-4342-A8FD-088FABF11F32}';
  IID_INetworkSettings:           TGUID = '{9F7DEA84-C30B-4245-80B6-00E9F646F1B4}';
  IID_IMaintenanceSettings:       TGUID = '{A6024FA8-9652-4ADB-A6BF-5CFCD877A7BA}';
  IID_ITaskSettings:              TGUID = '{8FD4711D-2D02-4c8c-87E3-EFF699DE127E}';
  IID_ITaskSettings2:             TGUID = '{2C05C3F0-6EED-4c05-A15F-ED7D7A98A369}';
  IID_ITaskSettings3:             TGUID = '{0AD9D0D7-0C7F-4EBB-9A5F-D1C648DCA528}';
  IID_IPrincipal:                 TGUID = '{D98D51E5-C9B4-496a-A9C1-18980261CF0F}';
  IID_IPrincipal2:                TGUID = '{248919AE-E345-4A6D-8AEB-E0D3165C904E}';
  IID_IAction:                    TGUID = '{BAE54997-48B1-4cbe-9965-D6BE263EBEA4}';
  IID_IActionCollection:          TGUID = '{02820E19-7B98-4ed2-B2E8-FDCCCEFF619B}';
  IID_IComHandlerAction:          TGUID = '{6D2FD252-75C5-4f66-90BA-2A7D8CC3039F}';
  IID_IEmailAction:               TGUID = '{10F62C64-7E16-4314-A0C2-0C3683F99D40}';
  IID_IExecAction:                TGUID = '{4c3d624d-fd6b-49a3-b9b7-09cb3cd3f047}';
  IID_IShowMessageAction:         TGUID = '{505E9E68-AF89-46b8-A30F-56162A83D537}';
  IID_ITaskDefinition:            TGUID = '{f5bc8fc5-536d-4f77-b852-fbc1356fdeb6}';
  IID_IRegisteredTask:            TGUID = '{9c86f320-dee3-4dd1-b972-a303f26b061e}';
  IID_IRegisteredTaskCollection:  TGUID = '{86627eb4-42a7-41e4-a4d9-ac33a72f2d52}';
  IID_ITaskFolder:                TGUID = '{8cfac062-a080-4c15-9a88-aa7c2af80dfc}';
  IID_ITaskFolderCollection:      TGUID = '{79184a66-8664-423f-97f1-637356a5d812}';
  IID_ITaskService:               TGUID = '{2faba4c7-4da9-4013-9697-20cc3fd40f85}';
  IID_ITaskHandler:               TGUID = '{839d7762-5121-4009-9234-4f0d19394f04}';
  IID_ITaskHandlerStatus:         TGUID = '{eaec7a8f-27a0-4ddc-8675-14726a01a38a}';
  IID_ITaskVariables:             TGUID = '{3e4c9351-d966-4b8b-bb87-ceba68bb0107}';


  CLSID_TaskScheduler:       TGUID = '{0f87369f-a4e5-4cfc-bd3e-73e6154572dd}';
  CLSID_TaskHandlerPS:       TGUID = '{f2a69db7-da2c-4352-9066-86fee6dacac9}';
  CLSID_TaskHandlerStatusPS: TGUID = '{9f15266d-d7ba-48f0-93c1-e6895f6fe5ac}';

type
  // forward declarations to resolve circular references
  ITaskFolderCollection = interface;  PITaskFolderCollection = ^ITaskFolderCollection;

//------------------------------------------------------------------------------
{
  https://msdn.microsoft.com/library/windows/desktop/aa381157
}
  PIRunningTask = ^IRunningTask;
  IRunningTask = interface(IDispatch)
  ['{653758fb-7b9a-4f1e-a471-beeb8e9b834e}']
    Function get_Name: BSTR; safecall;
    Function get_InstanceGuid: BSTR; safecall;
    Function get_Path: BSTR; safecall;
    Function get_State: TASK_STATE; safecall;
    Function get_CurrentAction: BSTR; safecall;
    Function Stop: HRESULT; stdcall;
    Function Refresh: HRESULT; stdcall;
    Function get_EnginePID: DWORD; safecall;
    property Name: BSTR read get_Name;
    property InstanceGuid: BSTR read get_InstanceGuid;
    property Path: BSTR read get_Path;
    property State: TASK_STATE read get_State;
    property CurrentAction: BSTR read get_CurrentAction;
    property EnginePID: DWORD read get_EnginePID;
  end;

//------------------------------------------------------------------------------
{
  https://msdn.microsoft.com/library/windows/desktop/aa381166
}
  PIRunningTaskCollection = ^IRunningTaskCollection;
  IRunningTaskCollection = interface(IDispatch)
  ['{6a67614b-6828-4fec-aa54-6d52e8f1f2db}']
    Function get_Count: LONG; safecall;
    Function get_Item(index: OleVariant): IRunningTask; safecall;
    Function get__NewEnum: IUnknown; safecall;
    property Count: LONG read get_Count;
    property Item[index: OleVariant]: IRunningTask read get_Item;
    property _NewEnum: IUnknown read get__NewEnum;
  end;

//------------------------------------------------------------------------------
{
  https://msdn.microsoft.com/library/windows/desktop/aa380773
}
  IRegistrationInfo = interface(IDispatch)
  ['{416D8B73-CB41-4ea1-805C-9BE9A5AC4A74}']
    Function get_Description: BSTR; safecall;
    procedure put_Description(description: BSTR); safecall;
    Function get_Author: BSTR; safecall;
    procedure put_Author(author: BSTR); safecall;
    Function get_Version: BSTR; safecall;
    procedure put_Version(version: BSTR); safecall;
    Function get_Date: BSTR; safecall;
    procedure put_Date(date: BSTR); safecall;
    Function get_Documentation: BSTR; safecall;
    procedure put_Documentation(documentation: BSTR); safecall;
    Function get_XmlText: BSTR; safecall;
    procedure put_XmlText(text: BSTR); safecall;
    Function get_URI: BSTR; safecall;
    procedure put_URI(Uri: BSTR); safecall;
    Function get_SecurityDescriptor: OleVariant; safecall;
    procedure put_SecurityDescriptor(sddl: OleVariant); safecall;
    Function get_Source: BSTR; safecall;
    procedure put_Source(source: BSTR); safecall;
    property Description: BSTR read get_Description write put_Description;
    property Author: BSTR read get_Author write put_Author;
    property Version: BSTR read get_Version write put_Version;
    property Date: BSTR read get_Date write put_Date;
    property Documentation: BSTR read get_Documentation write put_Documentation;
    property XmlText: BSTR read get_XmlText write put_XmlText;
    property URI: BSTR read get_URI write put_URI;
    property SecurityDescriptor: OleVariant read get_SecurityDescriptor write put_SecurityDescriptor;
    property Source: BSTR read get_Source write put_Source;
  end;

//------------------------------------------------------------------------------
{
  https://msdn.microsoft.com/library/windows/desktop/aa381128
}
  IRepetitionPattern = interface(IDispatch)
  ['{7FB9ACF1-26BE-400e-85B5-294B9C75DFD6}']
    Function get_Interval: BSTR; safecall;
    procedure put_Interval(interval: BSTR); safecall;
    Function get_Duration: BSTR; safecall;
    procedure put_Duration(duration: BSTR); safecall;
    Function get_StopAtDurationEnd: VARIANT_BOOL; safecall;
    procedure put_StopAtDurationEnd(stop: VARIANT_BOOL); safecall;
    property Interval: BSTR read get_Interval write put_Interval;
    property Duration: BSTR read get_Duration write put_Duration;
    property StopAtDurationEnd: VARIANT_BOOL read get_StopAtDurationEnd write put_StopAtDurationEnd;
  end;

//------------------------------------------------------------------------------
{
  https://msdn.microsoft.com/library/windows/desktop/aa381804
}
  PITaskNamedValuePair = ^ITaskNamedValuePair;
  ITaskNamedValuePair = interface(IDispatch)
  ['{39038068-2B46-4afd-8662-7BB6F868D221}']
    Function get_Name: BSTR; safecall;
    procedure put_Name(name: BSTR); safecall;
    Function get_Value: BSTR; safecall;
    procedure put_Value(value: BSTR); safecall;
    property Name: BSTR read get_Name write put_Name;
    property Value: BSTR read get_Value write put_Value;
  end;

//------------------------------------------------------------------------------
{
  https://msdn.microsoft.com/library/windows/desktop/aa381392
}
  ITaskNamedValueCollection = interface(IDispatch)
  ['{B4EF826B-63C3-46e4-A504-EF69E4F7EA4D}']
    Function get_Count: LONG; safecall;
    Function get_Item(index: LONG): ITaskNamedValuePair; safecall;
    Function get__NewEnum: IUnknown; safecall;
    Function Create(name, value: BSTR; ppPair: PITaskNamedValuePair): HRESULT; stdcall;
    Function Remove(index: LONG): HRESULT; stdcall;
    Function Clear: HRESULT; stdcall;
    property Count: LONG read get_Count;
    property Item[index: LONG]: ITaskNamedValuePair read get_Item;
    property _NewEnum: IUnknown read get__NewEnum;
  end;

//------------------------------------------------------------------------------
{
  https://msdn.microsoft.com/library/windows/desktop/aa381887
}
  PITrigger = ^ITrigger;
  ITrigger = interface(IDispatch)
  ['{09941815-ea89-4b5b-89e0-2a773801fac3}']
    Function get_Type: TASK_TRIGGER_TYPE2; safecall;
    Function get_Id: BSTR; safecall;
    procedure put_Id(id: BSTR); safecall;
    Function get_Repetition: IRepetitionPattern; safecall;
    procedure put_Repetition(pRepeat: IRepetitionPattern); safecall;
    Function get_ExecutionTimeLimit: BSTR; safecall;
    procedure put_ExecutionTimeLimit(timelimit: BSTR); safecall;
    Function get_StartBoundary: BSTR; safecall;
    procedure put_StartBoundary(start: BSTR); safecall;
    Function get_EndBoundary: BSTR; safecall;
    procedure put_EndBoundary(end_: BSTR); safecall;
    Function get_Enabled: VARIANT_BOOL; safecall;
    procedure put_Enabled(enabled: VARIANT_BOOL); safecall;
    property TriggerType: TASK_TRIGGER_TYPE2 read get_Type;
    property Id: BSTR read get_Id write put_Id;
    property Repetition: IRepetitionPattern read get_Repetition write put_Repetition;
    property ExecutionTimeLimit: BSTR read get_ExecutionTimeLimit write put_ExecutionTimeLimit;
    property StartBoundary: BSTR read get_StartBoundary write put_StartBoundary;
    property EndBoundary: BSTR read get_EndBoundary write put_EndBoundary;
    property Enabled: VARIANT_BOOL read get_Enabled write put_Enabled;
  end;

//------------------------------------------------------------------------------
{
  https://msdn.microsoft.com/library/windows/desktop/aa381889
}
  ITriggerCollection = interface(IDispatch)
  ['{85df5081-1b24-4f32-878a-d9d14df4cb77}']
    Function get_Count: LONG; safecall;
    Function get_Item(index: LONG): ITrigger; safecall;
    Function get__NewEnum: IUnknown; safecall;
    Function Create(type_: TASK_TRIGGER_TYPE2; ppTrigger: PITrigger): HRESULT; stdcall;
    Function Remove(index: OleVariant): HRESULT; stdcall;
    Function Clear: HRESULT; stdcall;
    property Count: LONG read get_Count;
    property Item[index: LONG]: ITrigger read get_Item;
    property _NewEnum: IUnknown read get__NewEnum;
  end;

//------------------------------------------------------------------------------
{
  https://msdn.microsoft.com/library/windows/desktop/aa380603
}
  IBootTrigger = interface(ITrigger)
  ['{2A9C35DA-D357-41f4-BBC1-207AC1B1F3CB}']
    Function get_Delay: BSTR; safecall;
    procedure put_Delay(delay: BSTR); safecall;
    property Delay: BSTR read get_Delay write put_Delay;
  end;

//------------------------------------------------------------------------------
{
  https://msdn.microsoft.com/library/windows/desktop/aa380656
}
  IDailyTrigger = interface(ITrigger)
  ['{126c5cd8-b288-41d5-8dbf-e491446adc5c}']
    Function get_DaysInterval: SHORT; safecall;
    procedure put_DaysInterval(days: SHORT); safecall;
    Function get_RandomDelay: BSTR; safecall;
    procedure put_RandomDelay(randomDelay: BSTR); safecall;
    property DaysInterval: SHORT read get_DaysInterval write put_DaysInterval;
    property RandomDelay: BSTR read get_RandomDelay write put_RandomDelay;
  end;

//------------------------------------------------------------------------------
{
  https://msdn.microsoft.com/library/windows/desktop/aa380711
}
  IEventTrigger = interface(ITrigger)
  ['{d45b0167-9653-4eef-b94f-0732ca7af251}']
    Function get_Subscription: BSTR; safecall;
    procedure put_Subscription(query: BSTR); safecall;
    Function get_Delay: BSTR; safecall;
    procedure put_Delay(delay: BSTR); safecall;
    Function get_ValueQueries: ITaskNamedValueCollection; safecall;
    procedure put_ValueQueries(pNamedXPaths: ITaskNamedValueCollection); safecall;
    property Subscription: BSTR read get_Subscription write put_Subscription;
    property Delay: BSTR read get_Delay write put_Delay;
    property ValueQueries: ITaskNamedValueCollection read get_ValueQueries write put_ValueQueries;
  end;

//------------------------------------------------------------------------------
{
  https://msdn.microsoft.com/library/windows/desktop/aa380724
}
  IIdleTrigger = interface(ITrigger)
  ['{d537d2b0-9fb3-4d34-9739-1ff5ce7b1ef3}']
  end;

//------------------------------------------------------------------------------
{
  https://msdn.microsoft.com/library/windows/desktop/aa380725
}
  ILogonTrigger = interface(ITrigger)
  ['{72DADE38-FAE4-4b3e-BAF4-5D009AF02B1C}']
    Function get_Delay: BSTR; safecall;
    procedure put_Delay(delay: BSTR); safecall;
    Function get_UserId: BSTR; safecall;
    procedure put_UserId(user: BSTR); safecall;
    property Delay: BSTR read get_Delay write put_Delay;
    property UserId: BSTR read get_UserId write put_UserId;
  end;

//------------------------------------------------------------------------------
{
  https://msdn.microsoft.com/library/windows/desktop/aa380728
}
  IMonthlyDOWTrigger = interface(ITrigger)
  ['{77d025a3-90fa-43aa-b52e-cda5499b946a}']
    Function get_DaysOfWeek: SHORT; safecall;
    procedure put_DaysOfWeek(days: SHORT); safecall;
    Function get_WeeksOfMonth: SHORT; safecall;
    procedure put_WeeksOfMonth(weeks: SHORT); safecall;
    Function get_MonthsOfYear: SHORT; safecall;
    procedure put_MonthsOfYear(months: SHORT); safecall;
    Function get_RunOnLastWeekOfMonth: VARIANT_BOOL; safecall;
    procedure put_RunOnLastWeekOfMonth(lastWeek: VARIANT_BOOL); safecall;
    Function get_RandomDelay: BSTR; safecall;
    procedure put_RandomDelay(randomDelay: BSTR); safecall; 
    property DaysOfWeek: SHORT read get_DaysOfWeek write put_DaysOfWeek;
    property WeeksOfMonth: SHORT read get_WeeksOfMonth write put_WeeksOfMonth;
    property MonthsOfYear: SHORT read get_MonthsOfYear write put_MonthsOfYear;
    property RunOnLastWeekOfMonth: VARIANT_BOOL read get_RunOnLastWeekOfMonth write put_RunOnLastWeekOfMonth;
    property RandomDelay: BSTR read get_RandomDelay write put_RandomDelay;
  end;

//------------------------------------------------------------------------------
{
  https://msdn.microsoft.com/library/windows/desktop/aa380734
}
  IMonthlyTrigger = interface(ITrigger)
  ['{97c45ef1-6b02-4a1a-9c0e-1ebfba1500ac}']
    Function get_DaysOfMonth: LONG; safecall;
    procedure put_DaysOfMonth(days: LONG); safecall;
    Function get_MonthsOfYear: SHORT; safecall;
    procedure put_MonthsOfYear(months: SHORT); safecall;
    Function get_RunOnLastDayOfMonth: VARIANT_BOOL; safecall;
    procedure put_RunOnLastDayOfMonth(lastDay: VARIANT_BOOL); safecall;
    Function get_RandomDelay: BSTR; safecall;
    procedure put_RandomDelay(randomDelay: BSTR); safecall;
    property DaysOfMonth: LONG read get_DaysOfMonth write put_DaysOfMonth;
    property MonthsOfYear: SHORT read get_MonthsOfYear write put_MonthsOfYear;
    property RunOnLastDayOfMonth: VARIANT_BOOL read get_RunOnLastDayOfMonth write put_RunOnLastDayOfMonth;
    property RandomDelay: BSTR read get_RandomDelay write put_RandomDelay;
  end;

//------------------------------------------------------------------------------
{
  https://msdn.microsoft.com/library/windows/desktop/aa381104
}
  IRegistrationTrigger = interface(ITrigger)
  ['{4c8fec3a-c218-4e0c-b23d-629024db91a2}']
    Function get_Delay: BSTR; safecall;
    procedure put_Delay(delay: BSTR); safecall;
    property Delay: BSTR read get_Delay write put_Delay;
  end;

//------------------------------------------------------------------------------
{
  https://msdn.microsoft.com/library/windows/desktop/aa381292
}
  ISessionStateChangeTrigger = interface(ITrigger)
  ['{754DA71B-4385-4475-9DD9-598294FA3641}']
    Function get_Delay: BSTR; safecall;
    procedure put_Delay(delay: BSTR); safecall;
    Function get_UserId: BSTR; safecall;
    procedure put_UserId(user: BSTR); safecall;
    Function get_StateChange: TASK_SESSION_STATE_CHANGE_TYPE; safecall;
    procedure put_StateChange(type_: TASK_SESSION_STATE_CHANGE_TYPE); safecall;
    property Delay: BSTR read get_Delay write put_Delay;
    property UserId: BSTR read get_UserId write put_UserId;
    property StateChange: TASK_SESSION_STATE_CHANGE_TYPE read get_StateChange write put_StateChange;
  end;

//------------------------------------------------------------------------------
{
  https://msdn.microsoft.com/library/windows/desktop/aa381885
}
  ITimeTrigger = interface(ITrigger)
  ['{b45747e0-eba7-4276-9f29-85c5bb300006}']
    Function get_RandomDelay: BSTR; safecall;
    procedure put_RandomDelay(randomDelay: BSTR); safecall;
    property RandomDelay: BSTR read get_RandomDelay write put_RandomDelay;
  end;

//------------------------------------------------------------------------------
{
  https://msdn.microsoft.com/library/windows/desktop/aa381904
}
  IWeeklyTrigger = interface(ITrigger)
  ['{5038fc98-82ff-436d-8728-a512a57c9dc1}']
    Function get_DaysOfWeek: SHORT; safecall;
    procedure put_DaysOfWeek(days: SHORT); safecall;
    Function get_WeeksInterval: SHORT; safecall;
    procedure put_WeeksInterval(weeks: SHORT); safecall;
    Function get_RandomDelay: BSTR; safecall;
    procedure put_RandomDelay(randomDelay: BSTR); safecall;
    property DaysOfWeek: SHORT read get_DaysOfWeek write put_DaysOfWeek;
    property WeeksInterval: SHORT read get_WeeksInterval write put_WeeksInterval;
    property RandomDelay: BSTR read get_RandomDelay write put_RandomDelay;
  end;

//------------------------------------------------------------------------------
{
  https://msdn.microsoft.com/library/windows/desktop/aa380719
}
  IIdleSettings = interface(IDispatch)
  ['{84594461-0053-4342-A8FD-088FABF11F32}']
    Function get_IdleDuration: BSTR; safecall;
    procedure put_IdleDuration(delay: BSTR); safecall;
    Function get_WaitTimeout: BSTR; safecall;
    procedure put_WaitTimeout(timeout: BSTR); safecall;
    Function get_StopOnIdleEnd: VARIANT_BOOL; safecall;
    procedure put_StopOnIdleEnd(stop: VARIANT_BOOL); safecall;
    Function get_RestartOnIdle: VARIANT_BOOL; safecall;
    procedure put_RestartOnIdle(restart: VARIANT_BOOL); safecall;
    property IdleDuration: BSTR read get_IdleDuration write put_IdleDuration;
    property WaitTimeout: BSTR read get_WaitTimeout write put_WaitTimeout;
    property StopOnIdleEnd: VARIANT_BOOL read get_StopOnIdleEnd write put_StopOnIdleEnd;
    property RestartOnIdle: VARIANT_BOOL read get_RestartOnIdle write put_RestartOnIdle;
  end;

//------------------------------------------------------------------------------
{
  https://msdn.microsoft.com/library/windows/desktop/aa380739
}
  INetworkSettings = interface(IDispatch)
  ['{9F7DEA84-C30B-4245-80B6-00E9F646F1B4}']
    Function get_Name: BSTR; safecall;
    procedure put_Name(name: BSTR); safecall;
    Function get_Id: BSTR; safecall;
    procedure put_Id(id: BSTR); safecall;
    property Name: BSTR read get_Name write put_Name;
    property Id: BSTR read get_Id write put_Id;
  end;

//------------------------------------------------------------------------------
{
  https://msdn.microsoft.com/library/windows/desktop/hh832144
}
  PIMaintenanceSettings = ^IMaintenanceSettings;
  IMaintenanceSettings = interface(IDispatch)
  ['{A6024FA8-9652-4ADB-A6BF-5CFCD877A7BA}']
    procedure  put_Period(value: BSTR); safecall;
    Function get_Period: BSTR; safecall;
    procedure put_Deadline(value: BSTR); safecall;
    Function get_Deadline: BSTR; safecall;
    procedure put_Exclusive(value: VARIANT_BOOL); safecall;
    Function get_Exclusive: VARIANT_BOOL; safecall;
    property Period: BSTR read get_Period write put_Period;
    property Deadline: BSTR read get_Deadline write put_Deadline;
    property Exclusive: VARIANT_BOOL read get_Exclusive write put_Exclusive;
  end;

//------------------------------------------------------------------------------
{
  https://msdn.microsoft.com/library/windows/desktop/aa381843
}
  ITaskSettings = interface(IDispatch)
  ['{8FD4711D-2D02-4c8c-87E3-EFF699DE127E}']
    Function get_AllowDemandStart: VARIANT_BOOL; safecall;
    procedure put_AllowDemandStart(allowDemandStart: VARIANT_BOOL); safecall;
    Function get_RestartInterval: BSTR; safecall;
    procedure put_RestartInterval(restartInterval: BSTR); safecall;
    Function get_RestartCount: int; safecall;
    procedure put_RestartCount(restartCount: int); safecall;
    Function get_MultipleInstances: TASK_INSTANCES_POLICY; safecall;
    procedure put_MultipleInstances(policy: TASK_INSTANCES_POLICY); safecall;
    Function get_StopIfGoingOnBatteries: VARIANT_BOOL; safecall;
    procedure put_StopIfGoingOnBatteries(stopIfOnBatteries: VARIANT_BOOL); safecall;
    Function get_DisallowStartIfOnBatteries: VARIANT_BOOL; safecall;
    procedure put_DisallowStartIfOnBatteries(disallowStart: VARIANT_BOOL); safecall;
    Function get_AllowHardTerminate: VARIANT_BOOL; safecall;
    procedure put_AllowHardTerminate(allowHardTerminate: VARIANT_BOOL); safecall;
    Function get_StartWhenAvailable: VARIANT_BOOL; safecall;
    procedure put_StartWhenAvailable(startWhenAvailable: VARIANT_BOOL); safecall;
    Function get_XmlText: BSTR; safecall;
    procedure put_XmlText(text: BSTR); safecall;
    Function get_RunOnlyIfNetworkAvailable: VARIANT_BOOL; safecall;
    procedure put_RunOnlyIfNetworkAvailable(runOnlyIfNetworkAvailable: VARIANT_BOOL); safecall;
    Function get_ExecutionTimeLimit: BSTR; safecall;
    procedure put_ExecutionTimeLimit(executionTimeLimit: BSTR); safecall;
    Function get_Enabled: VARIANT_BOOL; safecall;
    procedure put_Enabled(enabled: VARIANT_BOOL); safecall;
    Function get_DeleteExpiredTaskAfter: BSTR; safecall;
    procedure put_DeleteExpiredTaskAfter(expirationDelay: BSTR); safecall;
    Function get_Priority: int; safecall;
    procedure put_Priority(priority: int); safecall;
    Function get_Compatibility: TASK_COMPATIBILITY; safecall;
    procedure put_Compatibility(compatLevel: TASK_COMPATIBILITY); safecall;
    Function get_Hidden: VARIANT_BOOL; safecall;
    procedure put_Hidden(hidden: VARIANT_BOOL); safecall;
    Function get_IdleSettings: IIdleSettings; safecall;
    procedure put_IdleSettings(pIdleSettings: IIdleSettings); safecall;
    Function get_RunOnlyIfIdle: VARIANT_BOOL; safecall;
    procedure put_RunOnlyIfIdle(runOnlyIfIdle: VARIANT_BOOL); safecall;
    Function get_WakeToRun: VARIANT_BOOL; safecall;
    procedure put_WakeToRun(wake: VARIANT_BOOL); safecall;
    Function get_NetworkSettings: INetworkSettings; safecall;
    procedure put_NetworkSettings(pNetworkSettings: INetworkSettings); safecall;
    property AllowDemandStart: VARIANT_BOOL read get_AllowDemandStart write put_AllowDemandStart;
    property RestartInterval: BSTR read get_RestartInterval write put_RestartInterval;
    property RestartCount: int read get_RestartCount write put_RestartCount;
    property MultipleInstances: TASK_INSTANCES_POLICY read get_MultipleInstances write put_MultipleInstances;
    property StopIfGoingOnBatteries: VARIANT_BOOL read get_StopIfGoingOnBatteries write put_StopIfGoingOnBatteries;
    property DisallowStartIfOnBatteries: VARIANT_BOOL read get_DisallowStartIfOnBatteries write put_DisallowStartIfOnBatteries;
    property AllowHardTerminate: VARIANT_BOOL read get_AllowHardTerminate write put_AllowHardTerminate;
    property StartWhenAvailable: VARIANT_BOOL read get_StartWhenAvailable write put_StartWhenAvailable;
    property XmlText: BSTR read get_XmlText write put_XmlText;
    property RunOnlyIfNetworkAvailable: VARIANT_BOOL read get_RunOnlyIfNetworkAvailable write put_RunOnlyIfNetworkAvailable;
    property ExecutionTimeLimit: BSTR read get_ExecutionTimeLimit write put_ExecutionTimeLimit;
    property Enabled: VARIANT_BOOL read get_Enabled write put_Enabled;
    property DeleteExpiredTaskAfter: BSTR read get_DeleteExpiredTaskAfter write put_DeleteExpiredTaskAfter;
    property Priority: int read get_Priority write put_Priority;
    property Compatibility: TASK_COMPATIBILITY read get_Compatibility write put_Compatibility;
    property Hidden: VARIANT_BOOL read get_Hidden write put_Hidden;
    property IdleSettings: IIdleSettings read get_IdleSettings write put_IdleSettings;
    property RunOnlyIfIdle: VARIANT_BOOL read get_RunOnlyIfIdle write put_RunOnlyIfIdle;
    property WakeToRun: VARIANT_BOOL read get_WakeToRun write put_WakeToRun;
    property NetworkSettings: INetworkSettings read get_NetworkSettings write put_NetworkSettings;
  end;

//------------------------------------------------------------------------------
{
  https://msdn.microsoft.com/library/windows/desktop/ee695863
}
  ITaskSettings2 = interface(IDispatch)
  ['{2C05C3F0-6EED-4c05-A15F-ED7D7A98A369}']
    Function get_DisallowStartOnRemoteAppSession: VARIANT_BOOL; safecall;
    procedure put_DisallowStartOnRemoteAppSession(disallowStart: VARIANT_BOOL); safecall;
    Function get_UseUnifiedSchedulingEngine: VARIANT_BOOL; safecall;
    procedure put_UseUnifiedSchedulingEngine(useUnifiedEngine: VARIANT_BOOL); safecall;
    property DisallowStartOnRemoteAppSession: VARIANT_BOOL read get_DisallowStartOnRemoteAppSession write put_DisallowStartOnRemoteAppSession;
    property UseUnifiedSchedulingEngine: VARIANT_BOOL read get_UseUnifiedSchedulingEngine write put_UseUnifiedSchedulingEngine;
  end;

//------------------------------------------------------------------------------
{
  https://msdn.microsoft.com/library/windows/desktop/hh832148
}
  ITaskSettings3 = interface(ITaskSettings)
  ['{0AD9D0D7-0C7F-4EBB-9A5F-D1C648DCA528}']
    Function get_DisallowStartOnRemoteAppSession: VARIANT_BOOL; safecall;
    procedure put_DisallowStartOnRemoteAppSession(disallowStart: VARIANT_BOOL); safecall;
    Function get_UseUnifiedSchedulingEngine: VARIANT_BOOL; safecall;
    procedure put_UseUnifiedSchedulingEngine(useUnifiedEngine: VARIANT_BOOL); safecall;
    Function get_MaintenanceSettings: IMaintenanceSettings; safecall;
    procedure put_MaintenanceSettings(pMaintenanceSettings: IMaintenanceSettings); safecall;
    Function CreateMaintenanceSettings(ppMaintenanceSettings: PIMaintenanceSettings): HRESULT; stdcall;
    Function get_Volatile: VARIANT_BOOL; safecall;
    procedure put_Volatile(Volatile: VARIANT_BOOL); safecall;
    property DisallowStartOnRemoteAppSession: VARIANT_BOOL read get_DisallowStartOnRemoteAppSession write put_DisallowStartOnRemoteAppSession;
    property UseUnifiedSchedulingEngine: VARIANT_BOOL read get_UseUnifiedSchedulingEngine write put_UseUnifiedSchedulingEngine;
    property MaintenanceSettings: IMaintenanceSettings read get_MaintenanceSettings write put_MaintenanceSettings;
    property Volatile: VARIANT_BOOL read get_Volatile write put_Volatile;
  end;

//------------------------------------------------------------------------------
{
  https://msdn.microsoft.com/library/windows/desktop/aa380742
}
  IPrincipal = interface(IDispatch)
  ['{D98D51E5-C9B4-496a-A9C1-18980261CF0F}']
    Function get_Id: BSTR; safecall;
    procedure put_Id(Id: BSTR); safecall;
    Function get_DisplayName: BSTR; safecall;
    procedure put_DisplayName(name: BSTR); safecall;
    Function get_UserId: BSTR; safecall;
    procedure put_UserId(user: BSTR); safecall;
    Function get_LogonType: TASK_LOGON_TYPE; safecall;
    procedure put_LogonType(logon: TASK_LOGON_TYPE); safecall;
    Function get_GroupId: BSTR; safecall;
    procedure put_GroupId(group: BSTR); safecall;
    Function get_RunLevel: TASK_RUNLEVEL_TYPE; safecall;
    procedure put_RunLevel(runLevel: TASK_RUNLEVEL_TYPE); safecall;
    property Id: BSTR read get_Id write put_Id;
    property DisplayName: BSTR read get_DisplayName write put_DisplayName;
    property UserId: BSTR read get_UserId write put_UserId;
    property LogonType: TASK_LOGON_TYPE read get_LogonType write put_LogonType;
    property GroupId: BSTR read get_GroupId write put_GroupId;
    property RunLevel: TASK_RUNLEVEL_TYPE read get_RunLevel write put_RunLevel;
  end;

//------------------------------------------------------------------------------
{
  https://msdn.microsoft.com/library/windows/desktop/ee695858
}
  IPrincipal2 = interface(IDispatch)
  ['{248919AE-E345-4A6D-8AEB-E0D3165C904E}']
    Function get_ProcessTokenSidType: TASK_PROCESSTOKENSID_TYPE; safecall;
    procedure put_ProcessTokenSidType(processTokenSidType: TASK_PROCESSTOKENSID_TYPE); safecall;
    Function get_RequiredPrivilegeCount: LONG; safecall;
    Function get_RequiredPrivilege(index: LONG): BSTR; safecall;
    Function AddRequiredPrivilege(privilege: BSTR): HRESULT; stdcall;
    property ProcessTokenSidType: TASK_PROCESSTOKENSID_TYPE read get_ProcessTokenSidType write put_ProcessTokenSidType;
    property RequiredPrivilegeCount: LONG read get_RequiredPrivilegeCount;
    property RequiredPrivilege[index: LONG]: BSTR read get_RequiredPrivilege;
  end;

//------------------------------------------------------------------------------
{
  https://msdn.microsoft.com/library/windows/desktop/aa446895
}
  PIAction = ^IAction;
  IAction = interface(IDispatch)
  ['{BAE54997-48B1-4cbe-9965-D6BE263EBEA4}']
    Function get_Id: BSTR; safecall;
    procedure put_Id(Id: BSTR); safecall;
    Function get_Type: TASK_ACTION_TYPE; safecall;
    property Id: BSTR read get_Id write put_Id;
    property ActionType: TASK_ACTION_TYPE read get_Type;
  end;

//------------------------------------------------------------------------------
{
  https://msdn.microsoft.com/library/windows/desktop/aa446896
}
  IActionCollection = interface(IDispatch)
  ['{02820E19-7B98-4ed2-B2E8-FDCCCEFF619B}']
    Function get_Count: LONG; safecall;
    Function get_Item(index: LONG): IAction; safecall;
    Function get__NewEnum: IUnknown; safecall;
    Function get_XmlText: BSTR; safecall;
    procedure put_XmlText(text: BSTR); safecall;
    Function Create(actionType: TASK_ACTION_TYPE; ppAction: PIAction): HRESULT; stdcall;
    Function Remove(index: OleVariant): HRESULT; stdcall;
    Function Clear: HRESULT; stdcall;
    Function get_Context: BSTR; safecall;
    procedure put_Context(context: BSTR); safecall;
    property Count: LONG read get_Count;
    property Item[index: LONG]: IAction read get_Item;
    property _NewEnum: IUnknown read get__NewEnum;
    property XmlText: BSTR read get_XmlText write put_XmlText;
    property Context: BSTR read get_Context write put_Context;
  end;

//------------------------------------------------------------------------------
{
  https://msdn.microsoft.com/library/windows/desktop/aa380613
}
  IComHandlerAction = interface(IAction)
  ['{6D2FD252-75C5-4f66-90BA-2A7D8CC3039F}']
    Function get_ClassId: BSTR; safecall;
    procedure put_ClassId(clsid: BSTR); safecall;
    Function get_Data: BSTR; safecall;
    procedure put_Data(data: BSTR); safecall;
    property ClassId: BSTR read get_ClassId write put_ClassId;
    property Data: BSTR read get_Data write put_Data;
  end;

//------------------------------------------------------------------------------
{
  https://msdn.microsoft.com/library/windows/desktop/aa380693
}
  IEmailAction = interface(IAction)
  ['{10F62C64-7E16-4314-A0C2-0C3683F99D40}']
    Function get_Server: BSTR; safecall;
    procedure put_Server(server: BSTR); safecall;
    Function get_Subject: BSTR; safecall;
    procedure put_Subject(subject: BSTR); safecall;
    Function get_To: BSTR; safecall;
    procedure put_To(to_: BSTR); safecall;
    Function get_Cc: BSTR; safecall;
    procedure put_Cc(cc: BSTR); safecall;
    Function get_Bcc: BSTR; safecall;
    procedure put_Bcc(bcc: BSTR); safecall;
    Function get_ReplyTo: BSTR; safecall;
    procedure put_ReplyTo(replyTo: BSTR); safecall;
    Function get_From: BSTR; safecall;
    procedure put_From(from: BSTR); safecall;
    Function get_HeaderFields: ITaskNamedValueCollection; safecall;
    procedure put_HeaderFields(pHeaderFields: ITaskNamedValueCollection); safecall;
    Function get_Body: BSTR; safecall;
    procedure put_Body(body: BSTR); safecall;
    Function get_Attachments: PSAFEARRAY; safecall;
    procedure put_Attachments(pAttachements: PSAFEARRAY); safecall;
    property Server: BSTR read get_Server write put_Server;
    property Subject: BSTR read get_Subject write put_Subject;
    property To_: BSTR read get_To write put_To;
    property Cc: BSTR read get_Cc write put_Cc;
    property Bcc: BSTR read get_Bcc write put_Bcc;
    property ReplyTo: BSTR read get_ReplyTo write put_ReplyTo;
    property From: BSTR read get_From write put_From;
    property HeaderFields: ITaskNamedValueCollection read get_HeaderFields write put_HeaderFields;
    property Body: BSTR read get_Body write put_Body;
    property Attachments: PSAFEARRAY read get_Attachments write put_Attachments;
  end;

//------------------------------------------------------------------------------
{
  https://msdn.microsoft.com/library/windows/desktop/aa380715
}
  IExecAction = interface(IAction)
  ['{4c3d624d-fd6b-49a3-b9b7-09cb3cd3f047}']
    Function get_Path: BSTR; safecall;
    procedure put_Path(path: BSTR); safecall;
    Function get_Arguments: BSTR; safecall;
    procedure put_Arguments(argument: BSTR); safecall;
    Function get_WorkingDirectory: BSTR; safecall;
    procedure put_WorkingDirectory(workingDirectory: BSTR); safecall;
    property Path: BSTR read get_Path write put_Path;
    property Arguments: BSTR read get_Arguments write put_Arguments;
    property WorkingDirectory: BSTR read get_WorkingDirectory write put_WorkingDirectory;
  end;

//------------------------------------------------------------------------------
{
  https://msdn.microsoft.com/library/windows/desktop/aa381302
}
  IShowMessageAction = interface(IAction)
  ['{505E9E68-AF89-46b8-A30F-56162A83D537}']
    Function get_Title: BSTR; safecall;
    procedure put_Title(title: BSTR); safecall;
    Function get_MessageBody: BSTR; safecall;
    procedure put_MessageBody(messageBody: BSTR); safecall;
    property Title: BSTR read get_Title write put_Title;
    property MessageBody: BSTR read get_MessageBody write put_MessageBody;
  end;

//------------------------------------------------------------------------------
{
  https://msdn.microsoft.com/library/windows/desktop/aa381313
}
  PITaskDefinition = ^ITaskDefinition;
  ITaskDefinition = interface(IDispatch)
  ['{f5bc8fc5-536d-4f77-b852-fbc1356fdeb6}']
    Function get_RegistrationInfo: IRegistrationInfo; safecall;
    procedure put_RegistrationInfo(pRegistrationInfo: IRegistrationInfo); safecall;
    Function get_Triggers: ITriggerCollection; safecall;
    procedure put_Triggers(pTriggers: ITriggerCollection); safecall;
    Function get_Settings: ITaskSettings; safecall;
    procedure put_Settings(pSettings: ITaskSettings); safecall;
    Function get_Data: BSTR; safecall;
    procedure put_Data(data: BSTR); safecall;
    Function get_Principal: IPrincipal; safecall;
    procedure put_Principal(pPrincipal: IPrincipal); safecall;
    Function get_Actions: IActionCollection safecall;
    procedure put_Actions(pActions: IActionCollection); safecall;
    Function get_XmlText: BSTR; safecall;
    procedure put_XmlText(xml: BSTR); safecall;
    property RegistrationInfo: IRegistrationInfo read get_RegistrationInfo write put_RegistrationInfo;
    property Triggers: ITriggerCollection read get_Triggers write put_Triggers;
    property Settings: ITaskSettings read get_Settings write put_Settings;
    property Data: BSTR read get_Data write put_Data;
    property Principal: IPrincipal read get_Principal write put_Principal;
    property Actions: IActionCollection read get_Actions write put_Actions;
    property XmlText: BSTR read get_XmlText write put_XmlText;
  end;

//------------------------------------------------------------------------------
{
  https://msdn.microsoft.com/library/windows/desktop/aa380751
}
  PIRegisteredTask = ^IRegisteredTask;
  IRegisteredTask = interface(IDispatch)
  ['{9c86f320-dee3-4dd1-b972-a303f26b061e}']
    Function get_Name: BSTR; safecall;
    Function get_Path: BSTR; safecall;
    Function get_State: TASK_STATE; safecall;
    Function get_Enabled: VARIANT_BOOL; safecall;
    procedure put_Enabled(enabled: VARIANT_BOOL); safecall;
    Function Run(params: OleVariant; ppRunningTask: PIRunningTask): HRESULT; stdcall;
    Function RunEx(params: OleVariant; flags, sessionID: LONG; user: BSTR; ppRunningTask: PIRunningTask): HRESULT; stdcall;
    Function GetInstance(flags: LONG; ppRunningTasks: PIRunningTaskCollection): HRESULT; stdcall;
    Function get_LastRunTime: DATE; safecall;
    Function get_LastTaskResult: LONG; safecall;
    Function get_NumberOfMissedRuns: LONG; safecall;
    Function get_NextRunTime: DATE; safecall;
    Function get_Definition: ITaskDefinition; safecall;
    Function get_Xml: BSTR; safecall;
    Function GetSecurityDescriptor(securityInformation: LONG; pSddl: PBSTR): HRESULT; stdcall;
    Function SetSecurityDescriptor(sddl: BSTR; flags: LONG): HRESULT; stdcall;
    Function Stop(flags: LONG): HRESULT; stdcall;
    Function GetRunTimes(pstStart, pstEnd: LPSYSTEMTIME; pCount: PDWORD; pRunTimes: PLPSYSTEMTIME): HRESULT; stdcall;
    property Name: BSTR read get_Name;
    property Path: BSTR read get_Path;
    property State: TASK_STATE read get_State;
    property Enabled: VARIANT_BOOL read get_Enabled write put_Enabled;
    property LastRunTime: DATE read get_LastRunTime;
    property LastTaskResult: LONG read get_LastTaskResult;
    property NumberOfMissedRuns: LONG read get_NumberOfMissedRuns;
    property NextRunTime: DATE read get_NextRunTime;
    property Definition: ITaskDefinition read get_Definition;
    property Xml: BSTR read get_Xml;
  end;

//------------------------------------------------------------------------------
{
  https://msdn.microsoft.com/library/windows/desktop/aa380752
}
  PIRegisteredTaskCollection = ^IRegisteredTaskCollection;
  IRegisteredTaskCollection = interface(IDispatch)
  ['{86627eb4-42a7-41e4-a4d9-ac33a72f2d52}']
    Function get_Count: LONG; safecall;
    Function get_Item(index: OleVariant): IRegisteredTask; safecall;
    Function get__NewEnum: IUnknown; safecall;
    property Count: LONG read get_Count;
    property Item[index: OleVariant]: IRegisteredTask read get_Item;
    property _NewEnum: IUnknown read get__NewEnum;
  end;

//------------------------------------------------------------------------------
{
  https://msdn.microsoft.com/library/windows/desktop/aa381330
}
  PITaskFolder = ^ITaskFolder;
  ITaskFolder = interface(IDispatch)
  ['{8cfac062-a080-4c15-9a88-aa7c2af80dfc}']
    Function get_Name: BSTR; safecall;
    Function get_Path: BSTR; safecall;
    Function GetFolder(path: BSTR; ppFolder: PITaskFolder): HRESULT; stdcall;
    Function GetFolders(flags: LONG; ppFolders: PITaskFolderCollection): HRESULT; stdcall;
    Function CreateFolder(subFolderName: BSTR; sddl: OleVariant; ppFolder: PITaskFolder): HRESULT; stdcall;
    Function DeleteFolder(subFolderName: BSTR; flags: LONG): HRESULT; stdcall;
    Function GetTask(path: BSTR; ppTask: PIRegisteredTask): HRESULT; stdcall;
    Function GetTasks(flags: LONG; ppTasks: PIRegisteredTaskCollection): HRESULT; stdcall;
    Function DeleteTask(name: BSTR; flags: LONG): HRESULT; stdcall;
    Function RegisterTask(path, xmlText: BSTR; flags: LONG; userID, password: OleVariant; logonType: TASK_LOGON_TYPE;
              sddl: OleVariant; ppTask: PIRegisteredTask): HRESULT; stdcall;
    Function RegisterTaskDefinition(path: BSTR; pDefinition: ITaskDefinition; flags: LONG; userID, password: OleVariant;
              logonType: TASK_LOGON_TYPE; sddl: OleVariant; pptask: PIRegisteredTask): HRESULT; stdcall;
    Function GetSecurityDescriptor(securityInformation: LONG; pSddl: PBSTR): HRESULT; stdcall;
    Function SetSecurityDescriptor(sddl: BSTR; flags: LONG): HRESULT; stdcall;
    property Name: BSTR read get_Name;
    property Path: BSTR read get_Path;
  end;

//------------------------------------------------------------------------------
{
  https://msdn.microsoft.com/library/windows/desktop/aa381332
}
  ITaskFolderCollection = interface(IDispatch)
  ['{79184a66-8664-423f-97f1-637356a5d812}']
    Function get_Count: LONG; safecall;
    Function get_Item(index: OleVariant): ITaskFolder; safecall;
    Function get__NewEnum: IUnknown; safecall;
    property Count: LONG read get_Count;
    property Item[index: OleVariant]: ITaskFolder read get_Item;
    property _NewEnum: IUnknown read get__NewEnum;
  end;

//------------------------------------------------------------------------------
{
  https://msdn.microsoft.com/library/windows/desktop/
}
  ITaskService = interface(IDispatch)
  ['{2faba4c7-4da9-4013-9697-20cc3fd40f85}']
    Function GetFolder(path: BSTR; ppFolder: PITaskFolder): HRESULT; stdcall;
    Function GetRunningTasks(flags: LONG; ppRunningTasks: PIRunningTaskCollection): HRESULT; stdcall;
    Function NewTask(flags: DWORD; ppDefinition: PITaskDefinition): HRESULT; stdcall;
    Function Connect(serverName, user, domain, password: OleVariant): HRESULT; stdcall;
    Function get_Connected: VARIANT_BOOL; safecall;
    Function get_TargetServer: BSTR; safecall;
    Function get_ConnectedUser: BSTR; safecall;
    Function get_ConnectedDomain: BSTR; safecall;
    Function get_HighestVersion: DWORD; safecall;
    property Connected: VARIANT_BOOL read get_Connected;
    property TargetServer: BSTR read get_TargetServer;
    property ConnectedUser: BSTR read get_ConnectedUser;
    property ConnectedDomain: BSTR read get_ConnectedDomain;
    property HighestVersion: DWORD read get_HighestVersion;
  end;

//------------------------------------------------------------------------------
{
  https://msdn.microsoft.com/library/windows/desktop/aa381370
}
  ITaskHandler = interface(IUnknown)
  ['{839d7762-5121-4009-9234-4f0d19394f04}']
    Function Start(pHandlerServices: IUnknown; data: BSTR): HRESULT; stdcall;
    Function Stop(pRetCode: PHRESULT): HRESULT; stdcall;
    Function Pause: HRESULT; stdcall;        
    Function Resume: HRESULT; stdcall;
  end;

//------------------------------------------------------------------------------
{
  https://msdn.microsoft.com/library/windows/desktop/aa381373
}
  ITaskHandlerStatus = interface(IUnknown)
  ['{eaec7a8f-27a0-4ddc-8675-14726a01a38a}']
    Function UpdateStatus(percentComplete: SHORT; statusMessage: BSTR): HRESULT; stdcall;
    Function TaskCompleted(taskErrCode: HRESULT): HRESULT; stdcall;
  end;

//------------------------------------------------------------------------------
{
  https://msdn.microsoft.com/library/windows/desktop/aa381868
}
  ITaskVariables = interface(IUnknown)
  ['{3e4c9351-d966-4b8b-bb87-ceba68bb0107}']
    Function GetInput(pInput: PBSTR): HRESULT; stdcall;
    Function SetOutput(input: BSTR): HRESULT; stdcall;
    Function GetContext(pContext: PBSTR): HRESULT; stdcall;
  end;

implementation

end.
