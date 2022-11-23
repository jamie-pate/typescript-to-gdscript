// random imported external module.
// TODO: support reading .d.ts files
// deno-lint-ignore no-unused-vars
import { source } from 'common-tags';

import { int } from './int.js';
import { TeamId } from './team-id.js';
import {
  ImportedArray1,
  ImportedArray2,
  ImportedInterface,
  ImportedRecord1,
  ImportedRecord2,
} from './imported-interface.js';
// external imports are ignored
import { AnyKind } from './any-kind.js';

export enum SomeEnum {
  one,
  two,
  three,
}

export enum StringExprEnum {
  one = 'one',
  two = 'two',
  three = 'three',
}

export enum NumberExprEnum {
  one = 1,
  two = 2,
  three = 3,
}

export interface ExportedTestInterface {
  id: TeamId;
  strKey: string;
  floatKey: number;
  boolKey: boolean;
}

interface TestInterface {
  id: TeamId;
  strKey: string;
  floatKey: number;
  boolKey: boolean;
  optionalDate?: Date;
  nullableOptionalDate?: Date | null;
  date: Date;
  strLit: 'abcd';
  intLit: 1;
  floatLit: 1.0;
  strUnion: 'training' | 'full';
  intfUnion: AnyKind;
  trueLit: true;
  //bigIntLit: BigInt(9007199254740991);
  imported: ImportedInterface;
  recordObject: Record<string, TestInterface>;
  array: ImportedInterface[];
}

export interface TestInterfaceRoot {
  id: TeamId;
  intKey: int;
  strKey: string;
  objKey: TestInterface;
  exported: ExportedTestInterface;
  imported: ImportedInterface;
  importedRecord1: ImportedRecord1;
  importedRecord2: ImportedRecord2;
  importedArray1: ImportedArray1;
  importedArray2: ImportedArray2;
  importedOrNull: ImportedInterface | null;
  optionalIntOrNull?: int | null;
  optionalImportedOrNull?: ImportedInterface | null;

  someEnum: SomeEnum;
  strEnum: StringExprEnum;
  numEnum: NumberExprEnum;

  stringArray: string[];
  stringArray2: Array<string>;
  optionalArray?: int[];
  optionalRecord?: Record<string, string>;
  optionalNullableArrayOfStringOrNull?: Array<string | null> | null;

  stringOrNullArray: Array<string | null>;
  stringArrayOrNull: string[] | null;

  intArray: int[];
  objArray: TestInterface[];
  recordObject: Record<string, TestInterface>;
  orNull: string | null;
  objOrNull: TestInterface | null;

  // deno-lint-ignore no-explicit-any
  anyType: any;
  // deno-lint-ignore no-explicit-any
  anyOrNull: any | null;
  // deno-lint-ignore no-explicit-any
  optionalAnyOrNull?: any | null;

  tupleType: [string, string];
  // not supported
  // hetrogenousTupleType: [string, number];
}

/**
 * not supported, opt out with @typescript-to-gdscript-skip
 */
// @typescript-to-gdscript-skip
export interface GenericExtend<T extends TestInterface> {
  arrayOfExtendsTestInterface: T[];
}
