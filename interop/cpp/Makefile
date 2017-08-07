# Copyright (c) Microsoft. All rights reserved.
# Licensed under the MIT license. See LICENSE file in the project root for full license information.

all:
	../../build/compiler/build/gbc/gbc cpp ../idl/interop.bond --output gen
	clang -g --std=c++11 -lstdc++ -I ~/code/bond/build/cpp -I ~/code/bond/cpp/inc -I ~/code/bond/thirdparty/rapidjson/include -o interop *.cpp gen/*.cpp
