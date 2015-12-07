#!/usr/bin/env python

from __future__ import print_function
from __future__ import unicode_literals

import CppHeaderParser
import copy
import sys

expected = CppHeaderParser.CppHeader(sys.argv[1])
actual = CppHeaderParser.CppHeader(sys.argv[2])

def check(expr, msg, *args):
    if not expr:
        print(msg.format(*args))
        sys.exit(1)


# DEFINES

check(expected.defines == actual.defines, "incorrect include-guard, or too many #defines")


# TYPEDEFS

typedefs_not_found = copy.deepcopy(expected.typedefs)
for (new_type, old_type) in actual.typedefs.items():
    check(
        new_type in expected.typedefs,
        "`typedef {} {}` was not expected", old_type, new_type,
    )

    check(
        actual.typedefs[new_type] == expected.typedefs[new_type],
        "incorrect typedef:\nexpected: {!r}\nfound: {!r}",
        expected.typedefs[new_type], actual.typedefs[new_type],
    )

    typedefs_not_found.pop(new_type)

if len(typedefs_not_found) > 0:
    print("the following typedefs were expected but were not found:")
    for (new_type, old_type) in typedefs_not_found.items():
        print("typedef {} {};".format(old_type, new_type))
    sys.exit(1)


# ENUMS

enums_not_found = map(lambda e: { "name": e["name"], "values": e["values"] }, expected.enums)
for enum in actual.enums:
    check(
        enum['typedef'] is True,
        "line {}: enum {!r} was not `typedef`ed", enum['line_number'], enum['name'],
    )

    expected_enum = filter(lambda e: e['name'] == enum['name'], expected.enums)

    if len(expected_enum) > 1:
        raise Exception("internal error: multiple enums with name {!r} in 'correct' header file: {!r}".format(enum["name"], sys.argv[1]))

    check(
        len(expected_enum) > 0,
        "line {}: enum {!r} was not expected", enum["line_number"], enum["name"],
    )

    expected_enum = expected_enum[0]
    enums_not_found.remove({ "name": expected_enum["name"], "values": expected_enum["values"] })
    check(
        expected_enum["values"] == enum["values"],
        "line {}: enum {!r} has incorrect values:\nexpected: {!r}\nfound: {!r}",
        enum["line_number"], enum["name"], expected_enum["values"], enum["values"],
    )

if len(enums_not_found) > 0:
    print("the following enums were expected but were not found:")
    for enum in enums_not_found:
        print("{!r}: {!r}".format(enum["name"], enum["values"]))
    sys.exit(1)


# FUNCTIONS

funcs_not_found = map(lambda f: f["debug"], expected.functions)
for func in actual.functions:
    check(
        any(map(lambda f: f["debug"] == func["debug"], expected.functions)),
        "line {}: function not expected: {!r}", func["line_number"], func["debug"],
    )
    funcs_not_found.remove(func["debug"])

if len(funcs_not_found) > 0:
    print("the following functions were expected but were not found:")
    for func in funcs_not_found:
        print(func)
    sys.exit(1)


# STRUCTS

structs_not_found = copy.deepcopy(expected.classes)
for (key, struct) in actual.classes.items():
    expected_struct = filter(lambda s: s["name"] == struct["name"], expected.classes.values())

    if len(expected_struct) > 1:
        raise Exception("internal error: multiple structs with name {!r} in 'correct' header file: {!r}".format(struct["name"], sys.argv[1]))

    check(
        len(expected_struct) > 0,
        "line {}: struct {!r} was not expected", struct["line_number"], struct["name"],
    )

    expected_struct = expected_struct[0]

    values = map(lambda s: { "name": s["name"], "type": s["type"] }, struct["properties"]["public"])
    expected_values = map(lambda s: { "name": s["name"], "type": s["type"] }, expected_struct["properties"]["public"])
    check(
        values == expected_values,
        "line {}: struct {!r} has incorrect members:\nexpected: {!r}\nfound: {!r}\n",
        struct["line_number"], struct["name"], values, expected_values,
    )

    structs_not_found.pop(key)

if len(structs_not_found) > 0:
    print("the following structs were expected but not found:")
    for struct in structs_not_found.values():
        print("typedef struct {} {{".format(struct["name"]))
        for member in struct["properties"]["public"]:
            print("\t{} {};".format(member["type"], member["name"]))
        print("}} {};".format(struct["name"]))
    sys.exit(1)
