extends Reference
var id: TeamId
var int_key: int
var str_key: String
var obj_key: TestInterface
var string_array: String
var int_array: int
var obj_array: TestInterface
var record_object: Dictionary
var or_null: String
var obj_or_null: TestInterface


func _init(src: Dictionary = {}):
    update(src)

func update(src: Dictionary):
    # custom import logic can be added by extending this model

    id = data.id
    int_key = data.intKey
    str_key = data.strKey
    obj_key = data.objKey
    string_array = []
    for __item__ in src.stringArray:
        string_array.append(__item__)
    int_array = []
    for __item__ in src.intArray:
        int_array.append(__item__)
    obj_array = []
    for __item__ in src.objArray:
        obj_array.append(__item__)
    record_object = {}
    for __key__ in src.recordObject:
        record_object[__key__] = src.recordObject[__key__]
    or_null = data.orNull
    obj_or_null = data.objOrNull
    
