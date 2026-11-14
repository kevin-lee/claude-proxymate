/*
 * Replacements for the two OpenSSL functions that http4s-crypto imports, on
 * macOS only (issue #95).
 *
 * Why: http4s-ember-server always creates its WebSocket helpers, and their
 * handshake code calls Hash[F].digest from http4s-crypto. On Scala Native,
 * http4s-crypto declares EVP_Digest and EVP_get_digestbyname with
 * @link("crypto"), so the linker found Homebrew's libcrypto dylib on the build
 * machine and wrote its path into the proxy binary. On a Mac without that
 * Homebrew formula, dyld then stopped the proxy before main. The release
 * binary must not load a Homebrew dylib.
 *
 * How: these definitions are in an object file, so the linker uses them before
 * the symbols of libcrypto. Then no symbol of libcrypto is used, and
 * -Wl,-dead_strip_dylibs (set in build.sbt for macOS) removes the libcrypto
 * load command. The digests come from Apple CommonCrypto, which is part of
 * libSystem.
 *
 * Limit: only these two functions are supplied, and the pointer that
 * EVP_get_digestbyname returns is not a real OpenSSL EVP_MD. If a dependency
 * starts to import other EVP_* functions, libcrypto stays in the binary and
 * scripts/check-macos-load-commands.sh fails the build. Do not then give this
 * pointer to real OpenSSL functions.
 *
 * On other platforms (Linux, used by the `test` CI job) this file is empty,
 * and the real libcrypto is used.
 */
#if defined(__APPLE__)

#include <CommonCrypto/CommonDigest.h>
#include <stddef.h>
#include <stdint.h>
#include <string.h>

typedef enum {
    CLAUDEPROXYMATE_DIGEST_MD5,
    CLAUDEPROXYMATE_DIGEST_SHA1,
    CLAUDEPROXYMATE_DIGEST_SHA256,
    CLAUDEPROXYMATE_DIGEST_SHA512
} claudeproxymate_digest_alg;

typedef struct {
    const char *name;
    claudeproxymate_digest_alg alg;
} claudeproxymate_digest;

/* The names are exactly the ones that http4s-crypto HashPlatform.scala passes. */
static const claudeproxymate_digest claudeproxymate_md5 = {"MD5", CLAUDEPROXYMATE_DIGEST_MD5};
static const claudeproxymate_digest claudeproxymate_sha1 = {"SHA1", CLAUDEPROXYMATE_DIGEST_SHA1};
static const claudeproxymate_digest claudeproxymate_sha256 = {"SHA256", CLAUDEPROXYMATE_DIGEST_SHA256};
static const claudeproxymate_digest claudeproxymate_sha512 = {"SHA512", CLAUDEPROXYMATE_DIGEST_SHA512};

/* OpenSSL: const EVP_MD *EVP_get_digestbyname(const char *name) */
const void *EVP_get_digestbyname(const char *name) {
    if (name == NULL) {
        return NULL;
    }
    if (strcmp(name, claudeproxymate_md5.name) == 0) {
        return &claudeproxymate_md5;
    }
    if (strcmp(name, claudeproxymate_sha1.name) == 0) {
        return &claudeproxymate_sha1;
    }
    if (strcmp(name, claudeproxymate_sha256.name) == 0) {
        return &claudeproxymate_sha256;
    }
    if (strcmp(name, claudeproxymate_sha512.name) == 0) {
        return &claudeproxymate_sha512;
    }
    return NULL;
}

/* CC_LONG is 32 bits, so the data goes to Update in parts of not more than
 * UINT32_MAX bytes. */
static CC_LONG claudeproxymate_part_size(size_t remaining) {
    return remaining > (size_t)UINT32_MAX ? (CC_LONG)UINT32_MAX : (CC_LONG)remaining;
}

/* OpenSSL: int EVP_Digest(const void *data, size_t count, unsigned char *md,
 *                         unsigned int *size, const EVP_MD *type, ENGINE *impl) */
int EVP_Digest(const void *data, size_t count, unsigned char *md, unsigned int *size, const void *type,
               void *impl) {
    (void)impl;
    if (md == NULL || (data == NULL && count > 0)) {
        return 0;
    }

    const unsigned char *bytes = (const unsigned char *)data;
    size_t remaining = count;
    unsigned int length;

    if (type == &claudeproxymate_md5) {
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wdeprecated-declarations"
        CC_MD5_CTX ctx;
        CC_MD5_Init(&ctx);
        while (remaining > 0) {
            CC_LONG part = claudeproxymate_part_size(remaining);
            CC_MD5_Update(&ctx, bytes, part);
            bytes += part;
            remaining -= part;
        }
        CC_MD5_Final(md, &ctx);
#pragma clang diagnostic pop
        length = CC_MD5_DIGEST_LENGTH;
    } else if (type == &claudeproxymate_sha1) {
        CC_SHA1_CTX ctx;
        CC_SHA1_Init(&ctx);
        while (remaining > 0) {
            CC_LONG part = claudeproxymate_part_size(remaining);
            CC_SHA1_Update(&ctx, bytes, part);
            bytes += part;
            remaining -= part;
        }
        CC_SHA1_Final(md, &ctx);
        length = CC_SHA1_DIGEST_LENGTH;
    } else if (type == &claudeproxymate_sha256) {
        CC_SHA256_CTX ctx;
        CC_SHA256_Init(&ctx);
        while (remaining > 0) {
            CC_LONG part = claudeproxymate_part_size(remaining);
            CC_SHA256_Update(&ctx, bytes, part);
            bytes += part;
            remaining -= part;
        }
        CC_SHA256_Final(md, &ctx);
        length = CC_SHA256_DIGEST_LENGTH;
    } else if (type == &claudeproxymate_sha512) {
        CC_SHA512_CTX ctx;
        CC_SHA512_Init(&ctx);
        while (remaining > 0) {
            CC_LONG part = claudeproxymate_part_size(remaining);
            CC_SHA512_Update(&ctx, bytes, part);
            bytes += part;
            remaining -= part;
        }
        CC_SHA512_Final(md, &ctx);
        length = CC_SHA512_DIGEST_LENGTH;
    } else {
        return 0;
    }

    if (size != NULL) {
        *size = length;
    }
    return 1;
}

#endif /* defined(__APPLE__) */
