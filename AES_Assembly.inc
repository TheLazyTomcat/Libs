{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  Rijndael/AES cipher

    This file contains code fragments used in assembly implementation of
    hardware-accelerated AES (utilizing AESNI instructions).

  Version 2.0 (2021-04-05)

  Last change 2024-09-09

  ©2016-2024 František Milt

  Contacts:
    František Milt: frantisek.milt@gmail.com

  Support:
    If you find this code useful, please consider supporting its author(s) by
    making a small donation using the following link(s):

      https://www.paypal.me/FMilt

  Changelog:
    For detailed changelog and history please refer to this git repository:

      github.com/TheLazyTomcat/Lib.AES

  Dependencies:
    AuxTypes    - github.com/TheLazyTomcat/Lib.AuxTypes
    CipherBase  - github.com/TheLazyTomcat/Lib.CipherBase
  * SimpleCPUID - github.com/TheLazyTomcat/Lib.SimpleCPUID

  SimpleCPUID is required only when PurePascal symbol is not defined.

  Library SimpleCPUID might also be required as an indirect dependency.

  Indirect dependencies:
    AuxClasses         - github.com/TheLazyTomcat/Lib.AuxClasses
    AuxExceptions      - github.com/TheLazyTomcat/Lib.AuxExceptions
    StaticMemoryStream - github.com/TheLazyTomcat/Lib.StaticMemoryStream
    StrRect            - github.com/TheLazyTomcat/Lib.StrRect
    UInt64Utils        - github.com/TheLazyTomcat/Lib.UInt64Utils
    WinFileInfo        - github.com/TheLazyTomcat/Lib.WinFileInfo

===============================================================================}
{!tun_end!} // ignore this line

{$IFDEF KeyExpand_128_RoundCommon}
    SHUFPS    XMM1, XMM1, $FF
    PXOR      XMM1, XMM0
    PSLLDQ    XMM0, 4
    PXOR      XMM1, XMM0
    PSLLDQ    XMM0, 4
    PXOR      XMM1, XMM0
    PSLLDQ    XMM0, 4
    PXOR      XMM1, XMM0

    MOVAPS    XMM0, XMM1
    ADD       KeySchedule,  16
    MOVAPS    dqword ptr [KeySchedule],  XMM0
{$ENDIF}

//------------------------------------------------------------------------------

{$IFDEF KeyExpand_192_RoundCommon_1}
    SHUFPS    XMM3, XMM3, $FF
    PXOR      XMM3, XMM2
    PSLLDQ    XMM2, 4
    PXOR      XMM3, XMM2
    PSLLDQ    XMM2, 4
    PXOR      XMM3, XMM2
    PSLLDQ    XMM2, 4
    PXOR      XMM3, XMM2

    MOVAPS    dqword ptr [KeySchedule], XMM3
    ADD       KeySchedule,  16

    MOVAPS    XMM0, XMM3

    SHUFPS    XMM3, XMM3, $FF
    PSRLDQ    XMM1, 8
    PXOR      XMM3, XMM1
    PSLLDQ    XMM1, 4
    PXOR      XMM3, XMM1
    MOVAPS    XMM2, XMM3
    MOVAPS    XMM1, XMM0
{$ENDIF}

//------------------------------------------------------------------------------

{$IFDEF KeyExpand_192_RoundCommon_2}
    SHUFPS    XMM3, XMM3, $55
    PXOR      XMM3, XMM1
    PSLLDQ    XMM1, 4
    PXOR      XMM3, XMM1
    PSLLDQ    XMM3, 8
    MOVSD     XMM3, XMM2

    MOVAPS    dqword ptr [KeySchedule], XMM3
    ADD       KeySchedule,  16

    MOVAPS    XMM1, XMM3
    MOVHLPS   XMM2, XMM0
    MOVLHPS   XMM2, XMM1

    SHUFPS    XMM3, XMM3, $FF
    PXOR      XMM3, XMM2
    PSLLDQ    XMM2, 4
    PXOR      XMM3, XMM2
    PSLLDQ    XMM2, 4
    PXOR      XMM3, XMM2
    PSLLDQ    XMM2, 4
    PXOR      XMM3, XMM2

    MOVAPS    dqword ptr [KeySchedule], XMM3
    ADD       KeySchedule,  16

    MOVAPS    XMM0, XMM1
    MOVAPS    XMM1, XMM3
    MOVHLPS   XMM2, XMM0
    MOVLHPS   XMM2, XMM1
{$ENDIF}

//------------------------------------------------------------------------------

{$IFDEF KeyExpand_256_RoundCommon_1}
    SHUFPS    XMM2, XMM2, $FF
    PXOR      XMM2, XMM0
    PSLLDQ    XMM0, 4
    PXOR      XMM2, XMM0
    PSLLDQ    XMM0, 4
    PXOR      XMM2, XMM0
    PSLLDQ    XMM0, 4
    PXOR      XMM2, XMM0

    MOVAPS    XMM0, XMM2
    MOVAPS    dqword ptr [KeySchedule], XMM2
    ADD       KeySchedule,  16
{$ENDIF}

//------------------------------------------------------------------------------

{$IFDEF KeyExpand_256_RoundCommon_2}
    SHUFPS    XMM2, XMM2, $AA
    PXOR      XMM2, XMM1
    PSLLDQ    XMM1, 4
    PXOR      XMM2, XMM1
    PSLLDQ    XMM1, 4
    PXOR      XMM2, XMM1
    PSLLDQ    XMM1, 4
    PXOR      XMM2, XMM1

    MOVAPS    XMM1, XMM2
    MOVAPS    dqword ptr [KeySchedule], XMM2
    ADD       KeySchedule,  16
{$ENDIF}
