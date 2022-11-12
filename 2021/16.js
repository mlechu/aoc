// const raw = '220D69802BE00A0803711E1441B1006E39C318A12730C200DCE66D2CCE360FA0055652CD32966E3004677EDF600B0803B1361741510076254138D8A00E4FFF3E3393ABE4FC7AC10410010799D2A4430003764DBE281802F3102CA00D4840198430EE0E00021D04E3F41F84AE0154DFDE65A17CCBFAFA14ADA56854FE5E3FD5BCC53B0D2598027A00848C63F2B918C7E513DEC3290051B3867E009CCC5FE46BD520007FE5E8AD344B37583D0803E40085475887144C01A8C10FE2B9803B0720D45A3004652FD8FA05F80122CAF91E5F50E66BEF8AB000BB0F4802039C20917B920B9221200ABF0017B9C92CCDC76BD3A8C4012CCB13CB22CDB243E9C3D2002067440400D9BE62DAC4D2DC0249BF76B6F72BE459B279F759AE7BE42E0058801CC059B08018A0070012CEC045BA01006C03A8000D46C02FA000A8EA007200800E00618018E00410034220061801D36BF178C01796FC52B4017100763547E86000084C7E8910AC0027E9B029FE2F4952F96D81B34C8400C24AA8CDAF4F1E98027C00FACDE3BA86982570D13AA640195CD67B046F004662711E989C468C01F1007A10C4C8320008742287117C401A8C715A3FC2C8EB3777540048272DFE7DE1C0149AC8BC9E79D63200B674013978E8BE5E3A2E9AA3CCDD538C01193CFAB0A146006AA00087C3E88B130401D8E304A239802F39FAC922C0169EA3248DF2D600247C89BCDFE9CA7FFD8BB49686236C9FF9795D80C0139BEC4D6C017978CF78C5EB981FCE7D4D801FA9FB63B14789534584010B5802F3467346D2C1D1E080355B00424FC99290C7E5D729586504803A2D005E677F868C271AA479CEEB131592EE5450043A932697E6A92C6E164991EFC4268F25A294600B5002A3393B31CC834B972804D2F3A4FD72B928E59219C9C771EC3DC89D1802135C9806802729694A6E723FD6134C0129A019E600';
const raw = 'EE00D40C823060';
// const raw = '8A004A801A8002F478';
// const raw = '620080001611562C8802118E34';
// const raw = 'C0015000016115A2E0802F182340';
// const raw = 'A0016C880162017C3686B18A3D4780';
const hexToBin = {
  '0': '0000',
  '1': '0001',
  '2': '0010',
  '3': '0011',
  '4': '0100',
  '5': '0101',
  '6': '0110',
  '7': '0111',
  '8': '1000',
  '9': '1001',
  'A': '1010',
  'B': '1011',
  'C': '1100',
  'D': '1101',
  'E': '1110',
  'F': '1111',
}
const input = raw.split('').map(char => hexToBin[char]).join('');
console.log(input);

/**
 * @typedef {{version: number, type: number, tail: string}} Unread
 * @typedef {{version: number, length: number, type: 4, literal: number}} Literal
 * @typedef {{version: number, length: number, type: number, fn: (ps: Packet[]) => *, children: Packet[]}} Operator
 * @typedef {Literal | Operator} Packet
 */


/**
 * part 1
 */
const outer = readFirst(input);

function sumVersions(p = outer) {
  if (p.type === 4) return p.version;
  return p.version + readOperator(p).children.reduce((sum, c) => sum + sumVersions(c), 0);
}

console.log(sumVersions());
/**
 * @param {string} data
 * @returns {Unread}
 */
function readHeader(data) {
  return {
    version: parseInt(data.substring(0, 3), 2),
    type: parseInt(data.substring(3, 6), 2), // 4 = packet contains literal; other = operator
    tail: data.substring(6)
  };
}

/**
 * @param {Unread} unread
 * @returns {Literal}
 */
function readLiteral(unread) {

  /** @param {string} todo */
  function toLiteral(todo) {
    const first = todo.substring(1, 5);
    return todo[0] === '0' ?
      first
      : first + toLiteral(todo.substring(5));
  }

  const parsed = toLiteral(unread.tail);
  return { ...unread, length: 6 + parsed.length, literal: parseInt(parsed, 2) };
}

/**
 * @param {Unread} unread 
 * @returns {Operator}
 */
function readOperator(unread) {
  const lengthId = unread.tail[0];
  if (lengthId === '0') {
    // console.log(unread);
    const length = parseInt(unread.tail.substring(1, 16), 2);
    const packets = separatePackets(unread.tail.substring(16, 16 + length));
    return { ...unread, length: 16 + length, children: packets };
  } else if (lengthId === '1') {
    // console.log(unread);
    // console.log(unread.tail.substring(1, 12));
    const numChildren = parseInt(unread.tail.substring(1, 12), 2) || 0;
    const packets = separateNPackets(unread.tail.substring(12), numChildren);
    console.log(packets);
    const length = packets.reduce((total, p) => total + p.length, 0);
    return { ...unread, length: 12 + length, children: packets };
  }
}

/**
 * @param {string} data 
 * @returns {Packet[]}
 */
function separatePackets(data) {
  if (data.length < 11) return [];
  const first = readFirst(data);
  return [first, ...separatePackets(data.substring(first.length))];
}

/**
 * Don't know where the packets end within data
 * @param {string} data 
 * @param {number} n
 * @returns {Packet[]} where the sum of lengths can be used to determine list length
 */
function separateNPackets(data, n) {
  // console.log(n);
  return Array(n).fill(0).reduce((acc, _, i) => {
    const pos = i && acc.pos + acc.plist[i - 1];
    console.log(data.substring(pos))
    return {
      plist: acc.plist.concat(readFirst(data.substring(pos))),
      pos
    };
  },
    { plist: [], pos: 0 }
  ).plist;
}

/**
 * @param {string} data
 * @returns {Packet}
 */
function readFirst(data) {
  const unread = readHeader(data);
  console.log(unread);
  if (unread.type === 4) {
    const l = readLiteral(unread);
    // console.log(l);
    return l;
  } else {
    // console.log(readOperator(unread));
    const o = readOperator(unread);
    // console.log(o);
    return o;
  }
}

function align(n) {
  return n + n % 4
}