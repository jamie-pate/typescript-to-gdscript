import { int } from './int.js';
import { TeamId } from './team-id.js';

export interface TestInterface {
  id: TeamId;
  intKey: int;
  strKey: string;
  objKey: TestInterface;

  stringArray: string[];
  intArray: int[];
  objArray: TestInterface[];
  recordObject: Record<string, TestInterface>;
  orNull: string | null;
  objOrNull: TestInterface | null;
}
