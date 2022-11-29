import { v4 as uuidv4 } from 'uuid';
import * as rs from 'rs';

console.log("Before");
console.log(uuidv4());
rs.run_examples();
console.log("After");