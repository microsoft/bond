// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

// Disable some overeager MSC level 4 warnings.

// C4296: '<' : expression is always false
#pragma warning(disable: 4296)

// C4127: conditional expression is constant
#pragma warning(disable: 4127)

// C4503: decorated name length exceeded, name was truncated
#pragma warning(disable: 4503)

// C4702: unreachable code
#pragma warning(disable: 4702)

// C4103: alignment changed after including header, may be due to missing #pragma pack(pop)
#pragma warning(disable: 4103)

// C4482: nonstandard extension used: enum 'enum' used in qualified name
#pragma warning(disable: 4482)

// C4512: assignment operator could not be generated
#pragma warning(disable: 4512)
