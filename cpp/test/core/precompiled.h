#pragma once

#include <boost/mpl/for_each.hpp>
#include <boost/mpl/contains.hpp>
#include <boost/mpl/remove_if.hpp>
#include <boost/mpl/count_if.hpp>

#ifndef ENABLE_TEST_CASE
#   include <unit_test_core_apply.h>
#endif

#include <unit_test_types.h>
#include <unit_test_reflection.h>

using namespace unittest;

#include "unit_test_util.h"
