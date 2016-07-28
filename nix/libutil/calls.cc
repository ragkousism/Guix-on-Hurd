#include "config.h"

#include "calls.hh"

#include <iostream>
#include <cerrno>
#include <cstdio>
#include <cstdlib>
#include <sstream>
#include <cstring>

#include <sys/wait.h>
#include <unistd.h>
#include <fcntl.h>
#include <limits.h>
#include <argz.h>

#if __GNU__
#include <hurd/hurdutil.h>
#endif

namespace nix {

int nixMount(const char *source, const char *target,
  const char *filesystemtype, unsigned long mountflags,
  const void *data)
{
#if HAVE_SYS_MOUNT_H
    return mount(source, target, filesystemtype, mountflags, data);
#elif __GNU__
    struct settrans_context *context;
    int err;

    settrans_context_create(&context);

    context->node_name = strdup(target);

    if (filesystemtype == 0){
        argz_add(&context->argz, &context->argz_len, "hurd/ext2fs");
        argz_add(&context->argz, &context->argz_len, source);
    }

    if ((mountflags & MS_PRIVATE) == MS_PRIVATE){

    }

    if ((mountflags & MS_BIND) == MS_BIND){
        argz_add(&context->argz, &context->argz_len, "hurd/firmlink");
        argz_add(&context->argz, &context->argz_len, source);
    }

    context->active = 1;

    err = settrans(context);
    if (err){
        settrans_context_cleanup(context);
        return -1;
    }

    settrans_context_cleanup(context);

    return 0;
#else
    throw SysError("No Mount available on the system");
#endif
}

int nixUmount2(const char *target, int flags)
{
#if HAVE_SYS_MOUNT_H
    return umount2(target, flags);
#elif __GNU__
    struct settrans_context *context;
    int err;

    settrans_context_create(&context);

    context->node_name = strdup(target);
    context->kill_active = 1;

    if ((flags & MNT_FORCE) == MNT_FORCE){
        context->goaway_flags |= FSYS_GOAWAY_FORCE;
    }

    if ((flags & MNT_DETACH) == MNT_DETACH){

    }

    err = settrans(context);
    if (err){
        settrans_context_cleanup(context);
        return -1;
    }

    settrans_context_cleanup(context);

    return 0;
#else
    throw SysError("No Umount available on the system");
#endif
}

int pivot_root(const char *new_root, const char *put_old)
{
#if defined(SYS_pivot_root)
    return syscall(SYS_pivot_root, new_root,put_old);
#else
    return ENOSYS;
#endif
}

}
