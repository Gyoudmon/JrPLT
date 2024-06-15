#include <iostream> /* C++ 标准输入输出头文件 */

int main(int argc, char* argv[]) {
    /* 数组式初始化 */
    // 设 8-长度 字符数组，但初始化列表只提供了前5个元素，其余补'\0'
    char chars[8] = { 'c', 'h', 'a', 'r', 's' };
    // 设 字符串 str，无需指明长度，初始化列表里有多少算多少，也无需操心结尾的'\0'
    std::string str = { 's', 't', 'r', 'i', 'n', 'g' };
    // END

    /* 字符串式初始化 */
    char chrry[] = "char-array"; // 设 11-长度 字符数组 chrry，别漏了字符串的终止字符
    std::string s = "string";    // 设 字符串 s
    // END

    /** 插入操作
     * std::string& insert(int pos, std::string& str);
     * std::string& append(std::string& str);
     */
    s.insert(0, "<");               // 在字符串开头插入 "<"
    s.insert(s.length(), ">");      // 在字符串结尾插入 ">"

    // insert 方法返回的是字符串自己，因此可以把多个 insert 连续写在一起
    s.insert(1, "std").insert(4, "::");

    std::cout << s << std::endl;    // 输出: <std::string>
    // END

    /** 擦除操作
     * std::string& erase(int pos, int count);
     */
    s.erase(1, 5);                  // 从第1个位置处起连续删除5个字符

    std::cout << s << std::endl;    // 输出: <string>
    // END

    /** 替换操作
     * std::string& replace(int pos, int count, std::string& str);
     */
    s.replace(1, 6, chrry);         // 将第1个位置处起连6个字符替换成 chrry
    s[5] = ' ';                     // 别忘了最简单的替换操作就是赋值

    std::cout << s << std::endl;    // 输出: <char array>
    // END

    /** 查找操作
     * int find(std::string& substr, int pos = 0);
     */
    std::cout << s.find("char") << std::endl;    // 从头开始查找 “char”, 返回 1

    if (s.find(chars, 5) == std::string::npos) { // 从第5个位置开始查找 “chars”, 返回 npos
        std::cout << ("没找到" + std::string(chars)) << std::endl;
    }
    // END

    /** 比较操作
     * int compare(std::string& str);
     */
    std::cout << std::string("a").compare("A") << std::endl;        // 小写字母 > 大写字母
    std::cout << std::string("A").compare("9") << std::endl;        // 大写字母 > 数字
    std::cout << std::string("abc").compare("abcd") << std::endl;   // 前缀较小
    std::cout << std::string("1234").compare("123") << std::endl;   // 前缀较小
    std::cout << std::string("a").compare("123") << std::endl;      // 不看长短只比大小
    // END

    /** 最长字符串长度 */
    printf("0x%lx\n", str.max_size());
    // END 

    return 0;
}
