#include <subprocess.h>
#include <initializer_list>

#include <string>
#include <vector>



char *convert(const std::string & s)
{
   char *pc = new char[s.size()+1];
   std::strcpy(pc, s.c_str());
   return pc; 
}

int cmd(std::initializer_list<std::string>  args)
{
    std::vector<char*> vc;
    std::transform(args.begin(), args.end(), std::back_inserter(vc), convert);
    // vc.push_back(NULL);

    struct subprocess_s subprocess;
    int result = subprocess_create(&vc[0], 0, &subprocess);
    if (0 != result) {
        printf("error occurred.\n");
    } else {
        FILE* p_stdout = subprocess_stdout(&subprocess);
        char data[32];
        fgets(data, 32, p_stdout);
        printf("result: %s\n", data);
    }

    // std::cout << "vc.size(): " << vc.size() << std::endl;

    // for ( size_t i = 0 ; i < vc.size() ; i++ )
    //     std::cout << vc[i] << std::endl;

    for ( size_t i = 0 ; i < vc.size() ; i++ )
        delete [] vc[i];

    return 0;
}


int main(int argc, char* argv[])
{
    cmd({"/bin/bash", "--version"});

    return 0;
}