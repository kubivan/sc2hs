import re
import argparse

def snake_to_camel(snake_case):
    words = snake_case.split('_')
    return words[0].capitalize() + ''.join(word.capitalize() for word in words[1:])

def cpp_enum_to_haskell(enum_type_name, enum_contents):
    type_name = ''.join(word.capitalize() for word in enum_type_name.split('_'))

    enum_lines = enum_contents.strip().split('\n')

    haskell_lines = []

    # Inside the loop that processes each line of the enum
    for line in enum_lines:
        line = line.strip()
        if '=' in line and '//' in line:
            parts = line.split('=')
            enum_name = parts[0].strip()
            enum_value_comment = parts[1].split('//')[0].strip()

            enum_value = enum_value_comment.split(',')[0]
            #enum_value = 
            comment = parts[1].strip().replace('//','');
            print(f"enum_value_comment:  {enum_value} {comment}" )

            haskell_enum_name = snake_to_camel(enum_name)
            haskell_line = f"{haskell_enum_name}  -- {comment}"
            haskell_lines.append((haskell_enum_name, enum_value, haskell_line))
        elif '=' in line:
            parts = line.split('=')
            enum_name = parts[0].strip()
            enum_value = parts[1].strip()[:-1]
            #print(f"enum_value ={enum_value}")

            haskell_enum_name = snake_to_camel(enum_name)
            haskell_line = f"{haskell_enum_name}  -- {enum_value}"
            haskell_lines.append((haskell_enum_name, enum_value, haskell_line))

        #elif '//' in line:  # Handling comments like " // Terran"
        #    comment = line.split('//')[1].strip()
        #    haskell_line = f"-- {comment}"
        #    haskell_lines.append(("", "", haskell_line))


    haskell_declaration = f"data {type_name} =\n   " + '\n | '.join([line for _, _, line in haskell_lines]) + f"\n deriving (Show, Eq)"
    #TODO: handle commets only lines
    #haskell_declaration_lines = [f"   {line}" if line.strip().startswith("--") else f" | {line}" for _, _, line in haskell_lines]
    #haskell_declaration = f"data {type_name} =\n" + '\n'.join(haskell_declaration_lines) + f"\n deriving (Show, Eq)"

    from_enum_function = f"\n  --fromEnum :: {type_name} -> Int\n  fromEnum x = case x of\n" + '\n'.join([f"    {enum} -> {value}" for enum, value, _ in haskell_lines]) + "\n"

    to_enum_function = f"\n  --toEnum :: Int -> {type_name}\n  toEnum x = case x of\n" + '\n'.join([f"    {value} -> {enum}" for enum, value, _ in haskell_lines]) + "\n"

    instance_declaration = f"\n\ninstance Enum {type_name} where\n" + from_enum_function + to_enum_function

    haskell_code = haskell_declaration + instance_declaration

    return haskell_code

def main():
    parser = argparse.ArgumentParser(description='Convert C++ enum to Haskell data type.')
    parser.add_argument('--input', '-i', type=str, required=True, help='Input file containing C++ enum.')
    parser.add_argument('--output', '-o', type=str, default='output.hs', help='Output file for generated Haskell code.')

    args = parser.parse_args()

    with open(args.input, 'r') as input_file:
        cpp_content = input_file.read()

    enum_matches = re.finditer(r'enum\s+class\s+(\w+)\s*{([\s\S]*?)};', cpp_content)
    haskell_enums = []

    for match in enum_matches:
        enum_name = match.group(1)
        enum_contents = match.group(2)
        haskell_enum = cpp_enum_to_haskell(enum_name, enum_contents)
        haskell_enums.append(haskell_enum)

    with open(args.output, 'w') as output_file:
        output_file.write('\n\n'.join(haskell_enums))

if __name__ == '__main__':
    main()
