#include <maya/MPxCommand.h>
#include <maya/MFnPlugin.h>

struct ShimCommand : public MPxCommand
{
    struct FunctionTable {
        MStatus (*doIt)(void*, const MArgList&);
        void (*destruct)(void*);
    };

    ShimCommand(void* rustData, const FunctionTable& functions);
    ~ShimCommand() override;
    MStatus doIt(const MArgList& args) override;

    void* _rustData;
    FunctionTable _functions;

    static void* Create(void*, const FunctionTable&);
};
