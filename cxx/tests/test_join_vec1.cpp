#include <iostream>
#include <numeric>
#include <sstream>
#include <string>

int main()
{
    std::string str = "Hello!";
    std::vector<std::string> vec(10, str);

    std::stringstream ss;
    for (size_t i = 0; i < vec.size(); ++i) // v is your vector of string
    {
        if (i != 0)
            ss << " ";
        ss << vec[i];
    }
    std::string s = ss.str();
    std::cout << s << std::endl;
}
