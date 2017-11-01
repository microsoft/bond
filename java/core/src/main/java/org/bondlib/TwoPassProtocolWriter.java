// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package org.bondlib;

/**
 * Provides interface to a writer to pre-run for protocols (like Compact Binary v2) which need two passes.
 */
public interface TwoPassProtocolWriter extends ProtocolWriter {

    /**
     * Provide the first-pass writer.
     *
     * @return the first-pass writer
     */
    ProtocolWriter getFirstPassWriter();
}
