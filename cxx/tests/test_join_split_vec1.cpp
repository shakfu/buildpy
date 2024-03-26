#include <iostream>
// #include <numeric>
#include <sstream>
// #include <string>


/*! note: imput is assumed to not contain NUL characters */
// template <typename Input, typename Output,
//           typename Value = typename Output::value_type>
std::vector<std::string> split(std::string s, char delimiter)
{
    std::vector<std::string> output;
    for (auto cur = std::begin(s), beg = cur;; ++cur) {
        if (cur == std::end(s) || *cur == delimiter || !*cur) {
            output.insert(output.end(), std::string(beg, cur));
            if (cur == std::end(s) || !*cur)
                break;
            else
                beg = std::next(cur);
        }
    }
    return output;
}


std::string join(std::vector<std::string> elements, const char* const delimiter)
{
    std::ostringstream os;
    auto b = std::begin(elements);
    auto e = std::end(elements);

    if (b != e) {
        std::copy(b, std::prev(e), std::ostream_iterator<std::string>(os, delimiter));
        b = std::prev(e);
    }
    if (b != e) {
        os << *b;
    }

    return os.str();
}


int main()
{
    std::string str = "Hello!";
    std::vector<std::string> vec(10, str);


    std::string s = join(vec, " ");
    std::cout << s << std::endl;

    std::vector<std::string> v = split("hello.world", '.');
    std::cout << v[0] << std::endl;    
    std::cout << v[1] << std::endl;    
}
