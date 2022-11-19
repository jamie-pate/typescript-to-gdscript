// random imported external module.
// TODO: support reading .d.ts files
import { source } from "common-tags";

import { int } from "./int.js";
import { TeamId } from "./team-id.js";
import {
  ImportedInterface,
  ImportedRecord1,
  ImportedRecord2,
  ImportedArray1,
  ImportedArray2,
} from "./imported-interface.js";
// external imports are ignored
import { AnyKind } from "./any-kind.js";

// @TODO
export enum SomeEnum {
  one,
  two,
  three,
}

export enum StringExprEnum {
  one = "one",
  two = "two",
  three = "three",
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
  strLit: "abcd";
  intLit: 1;
  floatLit: 1.0;
  strUnion: "training" | "full";
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

  someEnum: SomeEnum;
  //TODO: strEnum: StringExprEnum;
  //TODO: numEnum: NumberExprEnum;

  stringArray: string[];
  stringArray2: Array<string>;
  optionalArray?: int[];
  optionalRecord?: Record<string, string>;

  stringOrNullArray: Array<string | null>;
  stringArrayOrNull: string[] | null;

  intArray: int[];
  objArray: TestInterface[];
  recordObject: Record<string, TestInterface>;
  orNull: string | null;
  objOrNull: TestInterface | null;
}

/**
 * not supported, opt out with @typescript-to-gdscript-skip
 */
// @typescript-to-gdscript-skip
export interface GenericExtend<T extends TestInterface> {
  arrayOfExtendsTestInterface: T[];
}
