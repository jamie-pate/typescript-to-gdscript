import { int } from './int.js';
import { TeamId } from './team-id.js';
import { ImportedInterface, ImportedRecord1, ImportedRecord2, ImportedArray1, ImportedArray2 } from './imported-interface.js';

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

  stringArray: string[];
  stringArray2: Array<string>;
  // TODO: these don't work
  stringOrNullArray: Array<string | null>;
  stringArrayOrNull: string[] | null;

  intArray: int[];
  objArray: TestInterface[];
  recordObject: Record<string, TestInterface>;
  orNull: string | null;
  objOrNull: TestInterface | null;
}
