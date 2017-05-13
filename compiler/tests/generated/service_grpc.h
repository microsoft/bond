
#pragma once

#include "service_reflection.h"
#include "service_types.h"
#include "basic_types_grpc.h"
#include "namespace_basic_types_grpc.h"

#include <bond/comm/message.h>
#include <bond/ext/grpc/bond_utils.h>
#include <bond/ext/grpc/unary_call.h>
#include <bond/ext/grpc/detail/service.h>
#include <bond/ext/grpc/detail/service_call_data.h>

#include <boost/optional/optional.hpp>

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4100 4267)
#endif

#include <grpc++/impl/codegen/async_unary_call.h>
#include <grpc++/impl/codegen/method_handler_impl.h>
#include <grpc++/impl/codegen/rpc_method.h>
#include <grpc++/impl/codegen/service_type.h>
#include <grpc++/impl/codegen/status.h>
#include <grpc++/impl/codegen/stub_options.h>

#ifdef _MSC_VER
#pragma warning (pop)
#endif

namespace tests
{

class Foo final
{
public:
    class StubInterface
    {
    public:
        virtual ~StubInterface() {}

        /* TODO stub interface (public) for event foo11 */

        /* TODO stub interface (public) for event foo12 */

        /* TODO stub interface (public) for event foo12_impl */

        /* TODO stub interface (public) for event foo13 */

        /* TODO stub interface (public) for event foo14 */

        /* TODO stub interface (public) for event foo15 */

        virtual ::grpc::Status foo21(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::bond::comm::message<void>* response) = 0;
        std::unique_ptr< ::grpc::ClientAsyncResponseReaderInterface< ::bond::comm::message<void>>> Asyncfoo21(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::grpc::CompletionQueue* cq)
        {
            return std::unique_ptr< ::grpc::ClientAsyncResponseReaderInterface< ::bond::comm::message<void>>>(Asyncfoo21Raw(context, request, cq));
        }

        virtual ::grpc::Status foo22(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::bond::comm::message<void>* response) = 0;
        std::unique_ptr< ::grpc::ClientAsyncResponseReaderInterface< ::bond::comm::message<void>>> Asyncfoo22(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::grpc::CompletionQueue* cq)
        {
            return std::unique_ptr< ::grpc::ClientAsyncResponseReaderInterface< ::bond::comm::message<void>>>(Asyncfoo22Raw(context, request, cq));
        }

        virtual ::grpc::Status foo23(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::BasicTypes>& request, ::bond::comm::message<void>* response) = 0;
        std::unique_ptr< ::grpc::ClientAsyncResponseReaderInterface< ::bond::comm::message<void>>> Asyncfoo23(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::BasicTypes>& request, ::grpc::CompletionQueue* cq)
        {
            return std::unique_ptr< ::grpc::ClientAsyncResponseReaderInterface< ::bond::comm::message<void>>>(Asyncfoo23Raw(context, request, cq));
        }

        virtual ::grpc::Status foo24(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::dummy>& request, ::bond::comm::message<void>* response) = 0;
        std::unique_ptr< ::grpc::ClientAsyncResponseReaderInterface< ::bond::comm::message<void>>> Asyncfoo24(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::dummy>& request, ::grpc::CompletionQueue* cq)
        {
            return std::unique_ptr< ::grpc::ClientAsyncResponseReaderInterface< ::bond::comm::message<void>>>(Asyncfoo24Raw(context, request, cq));
        }

        virtual ::grpc::Status foo31(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::bond::comm::message< ::tests::BasicTypes>* response) = 0;
        std::unique_ptr< ::grpc::ClientAsyncResponseReaderInterface< ::bond::comm::message< ::tests::BasicTypes>>> Asyncfoo31(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::grpc::CompletionQueue* cq)
        {
            return std::unique_ptr< ::grpc::ClientAsyncResponseReaderInterface< ::bond::comm::message< ::tests::BasicTypes>>>(Asyncfoo31Raw(context, request, cq));
        }

        virtual ::grpc::Status foo32(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::bond::comm::message< ::tests::BasicTypes>* response) = 0;
        std::unique_ptr< ::grpc::ClientAsyncResponseReaderInterface< ::bond::comm::message< ::tests::BasicTypes>>> Asyncfoo32(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::grpc::CompletionQueue* cq)
        {
            return std::unique_ptr< ::grpc::ClientAsyncResponseReaderInterface< ::bond::comm::message< ::tests::BasicTypes>>>(Asyncfoo32Raw(context, request, cq));
        }

        virtual ::grpc::Status foo33(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::BasicTypes>& request, ::bond::comm::message< ::tests::BasicTypes>* response) = 0;
        std::unique_ptr< ::grpc::ClientAsyncResponseReaderInterface< ::bond::comm::message< ::tests::BasicTypes>>> Asyncfoo33(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::BasicTypes>& request, ::grpc::CompletionQueue* cq)
        {
            return std::unique_ptr< ::grpc::ClientAsyncResponseReaderInterface< ::bond::comm::message< ::tests::BasicTypes>>>(Asyncfoo33Raw(context, request, cq));
        }

        virtual ::grpc::Status _rd_foo33(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::BasicTypes>& request, ::bond::comm::message< ::tests::BasicTypes>* response) = 0;
        std::unique_ptr< ::grpc::ClientAsyncResponseReaderInterface< ::bond::comm::message< ::tests::BasicTypes>>> Async_rd_foo33(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::BasicTypes>& request, ::grpc::CompletionQueue* cq)
        {
            return std::unique_ptr< ::grpc::ClientAsyncResponseReaderInterface< ::bond::comm::message< ::tests::BasicTypes>>>(Async_rd_foo33Raw(context, request, cq));
        }

        virtual ::grpc::Status foo34(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::dummy>& request, ::bond::comm::message< ::tests::BasicTypes>* response) = 0;
        std::unique_ptr< ::grpc::ClientAsyncResponseReaderInterface< ::bond::comm::message< ::tests::BasicTypes>>> Asyncfoo34(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::dummy>& request, ::grpc::CompletionQueue* cq)
        {
            return std::unique_ptr< ::grpc::ClientAsyncResponseReaderInterface< ::bond::comm::message< ::tests::BasicTypes>>>(Asyncfoo34Raw(context, request, cq));
        }

        virtual ::grpc::Status foo41(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::bond::comm::message< ::tests::dummy>* response) = 0;
        std::unique_ptr< ::grpc::ClientAsyncResponseReaderInterface< ::bond::comm::message< ::tests::dummy>>> Asyncfoo41(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::grpc::CompletionQueue* cq)
        {
            return std::unique_ptr< ::grpc::ClientAsyncResponseReaderInterface< ::bond::comm::message< ::tests::dummy>>>(Asyncfoo41Raw(context, request, cq));
        }

        virtual ::grpc::Status foo42(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::bond::comm::message< ::tests::dummy>* response) = 0;
        std::unique_ptr< ::grpc::ClientAsyncResponseReaderInterface< ::bond::comm::message< ::tests::dummy>>> Asyncfoo42(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::grpc::CompletionQueue* cq)
        {
            return std::unique_ptr< ::grpc::ClientAsyncResponseReaderInterface< ::bond::comm::message< ::tests::dummy>>>(Asyncfoo42Raw(context, request, cq));
        }

        virtual ::grpc::Status foo43(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::BasicTypes>& request, ::bond::comm::message< ::tests::dummy>* response) = 0;
        std::unique_ptr< ::grpc::ClientAsyncResponseReaderInterface< ::bond::comm::message< ::tests::dummy>>> Asyncfoo43(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::BasicTypes>& request, ::grpc::CompletionQueue* cq)
        {
            return std::unique_ptr< ::grpc::ClientAsyncResponseReaderInterface< ::bond::comm::message< ::tests::dummy>>>(Asyncfoo43Raw(context, request, cq));
        }

        virtual ::grpc::Status foo44(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::dummy>& request, ::bond::comm::message< ::tests::dummy>* response) = 0;
        std::unique_ptr< ::grpc::ClientAsyncResponseReaderInterface< ::bond::comm::message< ::tests::dummy>>> Asyncfoo44(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::dummy>& request, ::grpc::CompletionQueue* cq)
        {
            return std::unique_ptr< ::grpc::ClientAsyncResponseReaderInterface< ::bond::comm::message< ::tests::dummy>>>(Asyncfoo44Raw(context, request, cq));
        }

        virtual ::grpc::Status cq(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::bond::comm::message< ::tests::BasicTypes>* response) = 0;
        std::unique_ptr< ::grpc::ClientAsyncResponseReaderInterface< ::bond::comm::message< ::tests::BasicTypes>>> Asynccq(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::grpc::CompletionQueue* cq)
        {
            return std::unique_ptr< ::grpc::ClientAsyncResponseReaderInterface< ::bond::comm::message< ::tests::BasicTypes>>>(AsynccqRaw(context, request, cq));
        }

    private:
        /* TODO stub interface (private) for event foo11 */
        /* TODO stub interface (private) for event foo12 */
        /* TODO stub interface (private) for event foo12_impl */
        /* TODO stub interface (private) for event foo13 */
        /* TODO stub interface (private) for event foo14 */
        /* TODO stub interface (private) for event foo15 */
        virtual ::grpc::ClientAsyncResponseReaderInterface< ::bond::comm::message<void>>* Asyncfoo21Raw(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::grpc::CompletionQueue* cq) = 0;
        virtual ::grpc::ClientAsyncResponseReaderInterface< ::bond::comm::message<void>>* Asyncfoo22Raw(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::grpc::CompletionQueue* cq) = 0;
        virtual ::grpc::ClientAsyncResponseReaderInterface< ::bond::comm::message<void>>* Asyncfoo23Raw(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::BasicTypes>& request, ::grpc::CompletionQueue* cq) = 0;
        virtual ::grpc::ClientAsyncResponseReaderInterface< ::bond::comm::message<void>>* Asyncfoo24Raw(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::dummy>& request, ::grpc::CompletionQueue* cq) = 0;
        virtual ::grpc::ClientAsyncResponseReaderInterface< ::bond::comm::message< ::tests::BasicTypes>>* Asyncfoo31Raw(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::grpc::CompletionQueue* cq) = 0;
        virtual ::grpc::ClientAsyncResponseReaderInterface< ::bond::comm::message< ::tests::BasicTypes>>* Asyncfoo32Raw(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::grpc::CompletionQueue* cq) = 0;
        virtual ::grpc::ClientAsyncResponseReaderInterface< ::bond::comm::message< ::tests::BasicTypes>>* Asyncfoo33Raw(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::BasicTypes>& request, ::grpc::CompletionQueue* cq) = 0;
        virtual ::grpc::ClientAsyncResponseReaderInterface< ::bond::comm::message< ::tests::BasicTypes>>* Async_rd_foo33Raw(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::BasicTypes>& request, ::grpc::CompletionQueue* cq) = 0;
        virtual ::grpc::ClientAsyncResponseReaderInterface< ::bond::comm::message< ::tests::BasicTypes>>* Asyncfoo34Raw(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::dummy>& request, ::grpc::CompletionQueue* cq) = 0;
        virtual ::grpc::ClientAsyncResponseReaderInterface< ::bond::comm::message< ::tests::dummy>>* Asyncfoo41Raw(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::grpc::CompletionQueue* cq) = 0;
        virtual ::grpc::ClientAsyncResponseReaderInterface< ::bond::comm::message< ::tests::dummy>>* Asyncfoo42Raw(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::grpc::CompletionQueue* cq) = 0;
        virtual ::grpc::ClientAsyncResponseReaderInterface< ::bond::comm::message< ::tests::dummy>>* Asyncfoo43Raw(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::BasicTypes>& request, ::grpc::CompletionQueue* cq) = 0;
        virtual ::grpc::ClientAsyncResponseReaderInterface< ::bond::comm::message< ::tests::dummy>>* Asyncfoo44Raw(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::dummy>& request, ::grpc::CompletionQueue* cq) = 0;
        virtual ::grpc::ClientAsyncResponseReaderInterface< ::bond::comm::message< ::tests::BasicTypes>>* AsynccqRaw(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::grpc::CompletionQueue* cq) = 0;
    };

    class Stub final : public StubInterface
    {
    public:
        Stub(const std::shared_ptr< ::grpc::ChannelInterface>& channel);

        /* TODO stub implementation (public) for event foo11 */

        /* TODO stub implementation (public) for event foo12 */

        /* TODO stub implementation (public) for event foo12_impl */

        /* TODO stub implementation (public) for event foo13 */

        /* TODO stub implementation (public) for event foo14 */

        /* TODO stub implementation (public) for event foo15 */

        ::grpc::Status foo21(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::bond::comm::message<void>* response) override;
        std::unique_ptr< ::grpc::ClientAsyncResponseReader< ::bond::comm::message<void>>> Asyncfoo21(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::grpc::CompletionQueue* cq)
        {
            return std::unique_ptr< ::grpc::ClientAsyncResponseReader< ::bond::comm::message<void>>>(Asyncfoo21Raw(context, request, cq));
        }

        ::grpc::Status foo22(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::bond::comm::message<void>* response) override;
        std::unique_ptr< ::grpc::ClientAsyncResponseReader< ::bond::comm::message<void>>> Asyncfoo22(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::grpc::CompletionQueue* cq)
        {
            return std::unique_ptr< ::grpc::ClientAsyncResponseReader< ::bond::comm::message<void>>>(Asyncfoo22Raw(context, request, cq));
        }

        ::grpc::Status foo23(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::BasicTypes>& request, ::bond::comm::message<void>* response) override;
        std::unique_ptr< ::grpc::ClientAsyncResponseReader< ::bond::comm::message<void>>> Asyncfoo23(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::BasicTypes>& request, ::grpc::CompletionQueue* cq)
        {
            return std::unique_ptr< ::grpc::ClientAsyncResponseReader< ::bond::comm::message<void>>>(Asyncfoo23Raw(context, request, cq));
        }

        ::grpc::Status foo24(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::dummy>& request, ::bond::comm::message<void>* response) override;
        std::unique_ptr< ::grpc::ClientAsyncResponseReader< ::bond::comm::message<void>>> Asyncfoo24(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::dummy>& request, ::grpc::CompletionQueue* cq)
        {
            return std::unique_ptr< ::grpc::ClientAsyncResponseReader< ::bond::comm::message<void>>>(Asyncfoo24Raw(context, request, cq));
        }

        ::grpc::Status foo31(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::bond::comm::message< ::tests::BasicTypes>* response) override;
        std::unique_ptr< ::grpc::ClientAsyncResponseReader< ::bond::comm::message< ::tests::BasicTypes>>> Asyncfoo31(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::grpc::CompletionQueue* cq)
        {
            return std::unique_ptr< ::grpc::ClientAsyncResponseReader< ::bond::comm::message< ::tests::BasicTypes>>>(Asyncfoo31Raw(context, request, cq));
        }

        ::grpc::Status foo32(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::bond::comm::message< ::tests::BasicTypes>* response) override;
        std::unique_ptr< ::grpc::ClientAsyncResponseReader< ::bond::comm::message< ::tests::BasicTypes>>> Asyncfoo32(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::grpc::CompletionQueue* cq)
        {
            return std::unique_ptr< ::grpc::ClientAsyncResponseReader< ::bond::comm::message< ::tests::BasicTypes>>>(Asyncfoo32Raw(context, request, cq));
        }

        ::grpc::Status foo33(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::BasicTypes>& request, ::bond::comm::message< ::tests::BasicTypes>* response) override;
        std::unique_ptr< ::grpc::ClientAsyncResponseReader< ::bond::comm::message< ::tests::BasicTypes>>> Asyncfoo33(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::BasicTypes>& request, ::grpc::CompletionQueue* cq)
        {
            return std::unique_ptr< ::grpc::ClientAsyncResponseReader< ::bond::comm::message< ::tests::BasicTypes>>>(Asyncfoo33Raw(context, request, cq));
        }

        ::grpc::Status _rd_foo33(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::BasicTypes>& request, ::bond::comm::message< ::tests::BasicTypes>* response) override;
        std::unique_ptr< ::grpc::ClientAsyncResponseReader< ::bond::comm::message< ::tests::BasicTypes>>> Async_rd_foo33(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::BasicTypes>& request, ::grpc::CompletionQueue* cq)
        {
            return std::unique_ptr< ::grpc::ClientAsyncResponseReader< ::bond::comm::message< ::tests::BasicTypes>>>(Async_rd_foo33Raw(context, request, cq));
        }

        ::grpc::Status foo34(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::dummy>& request, ::bond::comm::message< ::tests::BasicTypes>* response) override;
        std::unique_ptr< ::grpc::ClientAsyncResponseReader< ::bond::comm::message< ::tests::BasicTypes>>> Asyncfoo34(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::dummy>& request, ::grpc::CompletionQueue* cq)
        {
            return std::unique_ptr< ::grpc::ClientAsyncResponseReader< ::bond::comm::message< ::tests::BasicTypes>>>(Asyncfoo34Raw(context, request, cq));
        }

        ::grpc::Status foo41(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::bond::comm::message< ::tests::dummy>* response) override;
        std::unique_ptr< ::grpc::ClientAsyncResponseReader< ::bond::comm::message< ::tests::dummy>>> Asyncfoo41(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::grpc::CompletionQueue* cq)
        {
            return std::unique_ptr< ::grpc::ClientAsyncResponseReader< ::bond::comm::message< ::tests::dummy>>>(Asyncfoo41Raw(context, request, cq));
        }

        ::grpc::Status foo42(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::bond::comm::message< ::tests::dummy>* response) override;
        std::unique_ptr< ::grpc::ClientAsyncResponseReader< ::bond::comm::message< ::tests::dummy>>> Asyncfoo42(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::grpc::CompletionQueue* cq)
        {
            return std::unique_ptr< ::grpc::ClientAsyncResponseReader< ::bond::comm::message< ::tests::dummy>>>(Asyncfoo42Raw(context, request, cq));
        }

        ::grpc::Status foo43(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::BasicTypes>& request, ::bond::comm::message< ::tests::dummy>* response) override;
        std::unique_ptr< ::grpc::ClientAsyncResponseReader< ::bond::comm::message< ::tests::dummy>>> Asyncfoo43(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::BasicTypes>& request, ::grpc::CompletionQueue* cq)
        {
            return std::unique_ptr< ::grpc::ClientAsyncResponseReader< ::bond::comm::message< ::tests::dummy>>>(Asyncfoo43Raw(context, request, cq));
        }

        ::grpc::Status foo44(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::dummy>& request, ::bond::comm::message< ::tests::dummy>* response) override;
        std::unique_ptr< ::grpc::ClientAsyncResponseReader< ::bond::comm::message< ::tests::dummy>>> Asyncfoo44(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::dummy>& request, ::grpc::CompletionQueue* cq)
        {
            return std::unique_ptr< ::grpc::ClientAsyncResponseReader< ::bond::comm::message< ::tests::dummy>>>(Asyncfoo44Raw(context, request, cq));
        }

        ::grpc::Status cq(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::bond::comm::message< ::tests::BasicTypes>* response) override;
        std::unique_ptr< ::grpc::ClientAsyncResponseReader< ::bond::comm::message< ::tests::BasicTypes>>> Asynccq(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::grpc::CompletionQueue* cq)
        {
            return std::unique_ptr< ::grpc::ClientAsyncResponseReader< ::bond::comm::message< ::tests::BasicTypes>>>(AsynccqRaw(context, request, cq));
        }

    private:
        std::shared_ptr< ::grpc::ChannelInterface> channel_;

        ::grpc::ClientAsyncResponseReader< ::bond::comm::message<void>>* Asyncfoo21Raw(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::grpc::CompletionQueue* cq) override;
        const ::grpc::RpcMethod rpcmethod_foo21_;

        ::grpc::ClientAsyncResponseReader< ::bond::comm::message<void>>* Asyncfoo22Raw(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::grpc::CompletionQueue* cq) override;
        const ::grpc::RpcMethod rpcmethod_foo22_;

        ::grpc::ClientAsyncResponseReader< ::bond::comm::message<void>>* Asyncfoo23Raw(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::BasicTypes>& request, ::grpc::CompletionQueue* cq) override;
        const ::grpc::RpcMethod rpcmethod_foo23_;

        ::grpc::ClientAsyncResponseReader< ::bond::comm::message<void>>* Asyncfoo24Raw(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::dummy>& request, ::grpc::CompletionQueue* cq) override;
        const ::grpc::RpcMethod rpcmethod_foo24_;

        ::grpc::ClientAsyncResponseReader< ::bond::comm::message< ::tests::BasicTypes>>* Asyncfoo31Raw(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::grpc::CompletionQueue* cq) override;
        const ::grpc::RpcMethod rpcmethod_foo31_;

        ::grpc::ClientAsyncResponseReader< ::bond::comm::message< ::tests::BasicTypes>>* Asyncfoo32Raw(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::grpc::CompletionQueue* cq) override;
        const ::grpc::RpcMethod rpcmethod_foo32_;

        ::grpc::ClientAsyncResponseReader< ::bond::comm::message< ::tests::BasicTypes>>* Asyncfoo33Raw(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::BasicTypes>& request, ::grpc::CompletionQueue* cq) override;
        const ::grpc::RpcMethod rpcmethod_foo33_;

        ::grpc::ClientAsyncResponseReader< ::bond::comm::message< ::tests::BasicTypes>>* Async_rd_foo33Raw(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::BasicTypes>& request, ::grpc::CompletionQueue* cq) override;
        const ::grpc::RpcMethod rpcmethod__rd_foo33_;

        ::grpc::ClientAsyncResponseReader< ::bond::comm::message< ::tests::BasicTypes>>* Asyncfoo34Raw(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::dummy>& request, ::grpc::CompletionQueue* cq) override;
        const ::grpc::RpcMethod rpcmethod_foo34_;

        ::grpc::ClientAsyncResponseReader< ::bond::comm::message< ::tests::dummy>>* Asyncfoo41Raw(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::grpc::CompletionQueue* cq) override;
        const ::grpc::RpcMethod rpcmethod_foo41_;

        ::grpc::ClientAsyncResponseReader< ::bond::comm::message< ::tests::dummy>>* Asyncfoo42Raw(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::grpc::CompletionQueue* cq) override;
        const ::grpc::RpcMethod rpcmethod_foo42_;

        ::grpc::ClientAsyncResponseReader< ::bond::comm::message< ::tests::dummy>>* Asyncfoo43Raw(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::BasicTypes>& request, ::grpc::CompletionQueue* cq) override;
        const ::grpc::RpcMethod rpcmethod_foo43_;

        ::grpc::ClientAsyncResponseReader< ::bond::comm::message< ::tests::dummy>>* Asyncfoo44Raw(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::dummy>& request, ::grpc::CompletionQueue* cq) override;
        const ::grpc::RpcMethod rpcmethod_foo44_;

        ::grpc::ClientAsyncResponseReader< ::bond::comm::message< ::tests::BasicTypes>>* AsynccqRaw(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::grpc::CompletionQueue* cq) override;
        const ::grpc::RpcMethod rpcmethod_cq_;
    };

    static std::unique_ptr<Stub> NewStub(const std::shared_ptr< ::grpc::ChannelInterface>& channel, const ::grpc::StubOptions& options = ::grpc::StubOptions());

    class Service : public ::bond::ext::gRPC::detail::service
    {
    public:
        Service()
        {
            AddMethod("/tests.Foo/foo11");
            AddMethod("/tests.Foo/foo12");
            AddMethod("/tests.Foo/foo12_impl");
            AddMethod("/tests.Foo/foo13");
            AddMethod("/tests.Foo/foo14");
            AddMethod("/tests.Foo/foo15");
            AddMethod("/tests.Foo/foo21");
            AddMethod("/tests.Foo/foo22");
            AddMethod("/tests.Foo/foo23");
            AddMethod("/tests.Foo/foo24");
            AddMethod("/tests.Foo/foo31");
            AddMethod("/tests.Foo/foo32");
            AddMethod("/tests.Foo/foo33");
            AddMethod("/tests.Foo/_rd_foo33");
            AddMethod("/tests.Foo/foo34");
            AddMethod("/tests.Foo/foo41");
            AddMethod("/tests.Foo/foo42");
            AddMethod("/tests.Foo/foo43");
            AddMethod("/tests.Foo/foo44");
            AddMethod("/tests.Foo/cq");
        }

        virtual ~Service() { }
        virtual void start(::grpc::ServerCompletionQueue* cq0) override
        {
            BOOST_ASSERT(cq0);

            /* TODO: init for event foo11 */
            /* TODO: init for event foo12 */
            /* TODO: init for event foo12_impl */
            /* TODO: init for event foo13 */
            /* TODO: init for event foo14 */
            /* TODO: init for event foo15 */
            _rd_foo21.emplace(this, 6, cq0, std::bind(&foo21, this, std::placeholders::_1));
            _rd_foo22.emplace(this, 7, cq0, std::bind(&foo22, this, std::placeholders::_1));
            _rd_foo23.emplace(this, 8, cq0, std::bind(&foo23, this, std::placeholders::_1));
            _rd_foo24.emplace(this, 9, cq0, std::bind(&foo24, this, std::placeholders::_1));
            _rd_foo31.emplace(this, 10, cq0, std::bind(&foo31, this, std::placeholders::_1));
            _rd_foo32.emplace(this, 11, cq0, std::bind(&foo32, this, std::placeholders::_1));
            _rd_foo330.emplace(this, 12, cq0, std::bind(&foo33, this, std::placeholders::_1));
            _rd__rd_foo33.emplace(this, 13, cq0, std::bind(&_rd_foo33, this, std::placeholders::_1));
            _rd_foo34.emplace(this, 14, cq0, std::bind(&foo34, this, std::placeholders::_1));
            _rd_foo41.emplace(this, 15, cq0, std::bind(&foo41, this, std::placeholders::_1));
            _rd_foo42.emplace(this, 16, cq0, std::bind(&foo42, this, std::placeholders::_1));
            _rd_foo43.emplace(this, 17, cq0, std::bind(&foo43, this, std::placeholders::_1));
            _rd_foo44.emplace(this, 18, cq0, std::bind(&foo44, this, std::placeholders::_1));
            _rd_cq.emplace(this, 19, cq0, std::bind(&cq, this, std::placeholders::_1));

            /* TODO: queue event foo11 */
            /* TODO: queue event foo12 */
            /* TODO: queue event foo12_impl */
            /* TODO: queue event foo13 */
            /* TODO: queue event foo14 */
            /* TODO: queue event foo15 */
            queue_receive(6, &_rd_foo21->_receivedCall->_context, &_rd_foo21->_receivedCall->_request, &_rd_foo21->_receivedCall->_responder, cq0, &_rd_foo21.get());
            queue_receive(7, &_rd_foo22->_receivedCall->_context, &_rd_foo22->_receivedCall->_request, &_rd_foo22->_receivedCall->_responder, cq0, &_rd_foo22.get());
            queue_receive(8, &_rd_foo23->_receivedCall->_context, &_rd_foo23->_receivedCall->_request, &_rd_foo23->_receivedCall->_responder, cq0, &_rd_foo23.get());
            queue_receive(9, &_rd_foo24->_receivedCall->_context, &_rd_foo24->_receivedCall->_request, &_rd_foo24->_receivedCall->_responder, cq0, &_rd_foo24.get());
            queue_receive(10, &_rd_foo31->_receivedCall->_context, &_rd_foo31->_receivedCall->_request, &_rd_foo31->_receivedCall->_responder, cq0, &_rd_foo31.get());
            queue_receive(11, &_rd_foo32->_receivedCall->_context, &_rd_foo32->_receivedCall->_request, &_rd_foo32->_receivedCall->_responder, cq0, &_rd_foo32.get());
            queue_receive(12, &_rd_foo330->_receivedCall->_context, &_rd_foo330->_receivedCall->_request, &_rd_foo330->_receivedCall->_responder, cq0, &_rd_foo330.get());
            queue_receive(13, &_rd__rd_foo33->_receivedCall->_context, &_rd__rd_foo33->_receivedCall->_request, &_rd__rd_foo33->_receivedCall->_responder, cq0, &_rd__rd_foo33.get());
            queue_receive(14, &_rd_foo34->_receivedCall->_context, &_rd_foo34->_receivedCall->_request, &_rd_foo34->_receivedCall->_responder, cq0, &_rd_foo34.get());
            queue_receive(15, &_rd_foo41->_receivedCall->_context, &_rd_foo41->_receivedCall->_request, &_rd_foo41->_receivedCall->_responder, cq0, &_rd_foo41.get());
            queue_receive(16, &_rd_foo42->_receivedCall->_context, &_rd_foo42->_receivedCall->_request, &_rd_foo42->_receivedCall->_responder, cq0, &_rd_foo42.get());
            queue_receive(17, &_rd_foo43->_receivedCall->_context, &_rd_foo43->_receivedCall->_request, &_rd_foo43->_receivedCall->_responder, cq0, &_rd_foo43.get());
            queue_receive(18, &_rd_foo44->_receivedCall->_context, &_rd_foo44->_receivedCall->_request, &_rd_foo44->_receivedCall->_responder, cq0, &_rd_foo44.get());
            queue_receive(19, &_rd_cq->_receivedCall->_context, &_rd_cq->_receivedCall->_request, &_rd_cq->_receivedCall->_responder, cq0, &_rd_cq.get());
        }

        /* TODO: abstract method for event foo11 */
        /* TODO: abstract method for event foo12 */
        /* TODO: abstract method for event foo12_impl */
        /* TODO: abstract method for event foo13 */
        /* TODO: abstract method for event foo14 */
        /* TODO: abstract method for event foo15 */
        virtual void(::bond::ext::gRPC::unary_call<::bond::comm::message<void>, ::bond::comm::message<void>) = 0;
        virtual void(::bond::ext::gRPC::unary_call<::bond::comm::message<void>, ::bond::comm::message<void>) = 0;
        virtual void(::bond::ext::gRPC::unary_call<::bond::comm::message< ::tests::BasicTypes>, ::bond::comm::message<void>) = 0;
        virtual void(::bond::ext::gRPC::unary_call<::bond::comm::message< ::tests::dummy>, ::bond::comm::message<void>) = 0;
        virtual void(::bond::ext::gRPC::unary_call<::bond::comm::message<void>, ::bond::comm::message< ::tests::BasicTypes>) = 0;
        virtual void(::bond::ext::gRPC::unary_call<::bond::comm::message<void>, ::bond::comm::message< ::tests::BasicTypes>) = 0;
        virtual void(::bond::ext::gRPC::unary_call<::bond::comm::message< ::tests::BasicTypes>, ::bond::comm::message< ::tests::BasicTypes>) = 0;
        virtual void(::bond::ext::gRPC::unary_call<::bond::comm::message< ::tests::BasicTypes>, ::bond::comm::message< ::tests::BasicTypes>) = 0;
        virtual void(::bond::ext::gRPC::unary_call<::bond::comm::message< ::tests::dummy>, ::bond::comm::message< ::tests::BasicTypes>) = 0;
        virtual void(::bond::ext::gRPC::unary_call<::bond::comm::message<void>, ::bond::comm::message< ::tests::dummy>) = 0;
        virtual void(::bond::ext::gRPC::unary_call<::bond::comm::message<void>, ::bond::comm::message< ::tests::dummy>) = 0;
        virtual void(::bond::ext::gRPC::unary_call<::bond::comm::message< ::tests::BasicTypes>, ::bond::comm::message< ::tests::dummy>) = 0;
        virtual void(::bond::ext::gRPC::unary_call<::bond::comm::message< ::tests::dummy>, ::bond::comm::message< ::tests::dummy>) = 0;
        virtual void(::bond::ext::gRPC::unary_call<::bond::comm::message<void>, ::bond::comm::message< ::tests::BasicTypes>) = 0;

    private:
        /* TODO: receive data for event foo11 */
        /* TODO: receive data for event foo12 */
        /* TODO: receive data for event foo12_impl */
        /* TODO: receive data for event foo13 */
        /* TODO: receive data for event foo14 */
        /* TODO: receive data for event foo15 */
        boost::optional<::bond::ext::gRPC::detail::service_unary_call_data<::bond::comm::message<void>>, ::bond::comm::message<void>>>> _rd_foo21;
        boost::optional<::bond::ext::gRPC::detail::service_unary_call_data<::bond::comm::message<void>>, ::bond::comm::message<void>>>> _rd_foo22;
        boost::optional<::bond::ext::gRPC::detail::service_unary_call_data<::bond::comm::message< ::tests::BasicTypes>>, ::bond::comm::message<void>>>> _rd_foo23;
        boost::optional<::bond::ext::gRPC::detail::service_unary_call_data<::bond::comm::message< ::tests::dummy>>, ::bond::comm::message<void>>>> _rd_foo24;
        boost::optional<::bond::ext::gRPC::detail::service_unary_call_data<::bond::comm::message<void>>, ::bond::comm::message< ::tests::BasicTypes>>>> _rd_foo31;
        boost::optional<::bond::ext::gRPC::detail::service_unary_call_data<::bond::comm::message<void>>, ::bond::comm::message< ::tests::BasicTypes>>>> _rd_foo32;
        boost::optional<::bond::ext::gRPC::detail::service_unary_call_data<::bond::comm::message< ::tests::BasicTypes>>, ::bond::comm::message< ::tests::BasicTypes>>>> _rd_foo330;
        boost::optional<::bond::ext::gRPC::detail::service_unary_call_data<::bond::comm::message< ::tests::BasicTypes>>, ::bond::comm::message< ::tests::BasicTypes>>>> _rd__rd_foo33;
        boost::optional<::bond::ext::gRPC::detail::service_unary_call_data<::bond::comm::message< ::tests::dummy>>, ::bond::comm::message< ::tests::BasicTypes>>>> _rd_foo34;
        boost::optional<::bond::ext::gRPC::detail::service_unary_call_data<::bond::comm::message<void>>, ::bond::comm::message< ::tests::dummy>>>> _rd_foo41;
        boost::optional<::bond::ext::gRPC::detail::service_unary_call_data<::bond::comm::message<void>>, ::bond::comm::message< ::tests::dummy>>>> _rd_foo42;
        boost::optional<::bond::ext::gRPC::detail::service_unary_call_data<::bond::comm::message< ::tests::BasicTypes>>, ::bond::comm::message< ::tests::dummy>>>> _rd_foo43;
        boost::optional<::bond::ext::gRPC::detail::service_unary_call_data<::bond::comm::message< ::tests::dummy>>, ::bond::comm::message< ::tests::dummy>>>> _rd_foo44;
        boost::optional<::bond::ext::gRPC::detail::service_unary_call_data<::bond::comm::message<void>>, ::bond::comm::message< ::tests::BasicTypes>>>> _rd_cq;
    };
};

} // namespace tests

