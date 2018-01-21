#include "shim.h"

ShimCommand::ShimCommand(void* rustData, const FunctionTable& functions)
    : _rustData(rustData), _functions(functions) {}
ShimCommand::~ShimCommand()
{
    _functions.destruct(_rustData);
}
MStatus ShimCommand::doIt(const MArgList& args)
{
    return _functions.doIt(_rustData, args);
}
void* ShimCommand::Create(void* rustData, const FunctionTable& functions)
{
    return new ShimCommand(rustData, functions);
}
